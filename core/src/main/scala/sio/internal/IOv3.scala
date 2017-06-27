package sio.internal

import scala.annotation.tailrec
import scala.util.control.NonFatal

object IOv3 {
  trait IOv3Interface {
    type T[+A]

    def unit: T[Unit]
    def pure[A](a: A): T[A]
    def raise(e: Throwable): T[Nothing]
    def delay[A](a: Any => A): T[A]

    def map[A, B](fa: T[A])(f: A => B): T[B]
    def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B]
    def handle[A](fa: T[A])(f: Throwable => T[A]): T[A]

    def run[A](action: T[A]): Either[Throwable, A]
  }

  private[IOv3] object detail {
    sealed abstract class Thunk extends Product with Serializable

    final case class Pure  (v: Any) extends Thunk
    final case class Raise (e: Throwable) extends Thunk

    final case class Delay (f: Any => Any) extends Thunk
    final case class Map   (f: Any => Any,         tail: Thunk) extends Thunk
    final case class Bind  (f: Any => Thunk,       tail: Thunk) extends Thunk
    final case class Handle(f: Throwable => Thunk, tail: Thunk) extends Thunk
  }
  import detail._

  final class IOv3Implementation extends IOv3Interface {
    val EAGER_PURE_EVAL: Boolean = true
    val HAPPY_PATH_DEPTH: Int    = 1024

    type T[+A] = Thunk

    val unit: T[Unit]                     = Pure(())
    def pure[A](a: A): T[A]               = Pure(a)
    def raise(e: Throwable): T[Nothing]   = Raise(e)
    def delay[A](a: Any => A): T[A]       = Delay(a)

    def map[A, B](fa: T[A])(f: A => B): T[B] = {
      val ff = f.asInstanceOf[Any => Any]

      if (EAGER_PURE_EVAL) {
        fa match {
          case Pure(x)    => Pure(ff(x))
          case r@Raise(_) => r
          case _          => Map(ff, fa)
        }
      } else Map(ff, fa)
    }

    def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B] = {
      val ff = f.asInstanceOf[Any => Thunk]

      if (EAGER_PURE_EVAL) {
        fa match {
          case Pure(x)    => ff(x)
          case r@Raise(_) => r
          case _          => Bind(ff, fa)
        }
      } else Bind(ff, fa)
    }

    def handle[A](fa: T[A])(f: Throwable => T[A]): T[A] = {
      if (EAGER_PURE_EVAL) {
        fa match {
          case p@Pure(_) => p
          case Raise(e)  => f(e)
          case _         => Handle(f, fa)
        }
      } else Handle(f, fa)
    }

    def run[A](action: T[A]): Either[Throwable, A] = {
      val rts = new RTS
      rts.run(action)
    }

    final class RTS {
      @inline val OP_PURE   = 0
      @inline val OP_MAP    = 1
      @inline val OP_BIND   = 2
      @inline val OP_HANDLE = 3
      @inline val OP_DELAY  = 4

      var queue: Array[Any] = Array.ofDim[Any](16)
      var ops  : Array[Int] = Array.ofDim[Int](16)
      var last : Int = -1

      def append(opi: Int, op: Any): Unit = {
        if (last + 1 >= queue.length) {
          val newCapacity = queue.length * 3 / 2
          val newQueue = Array.ofDim[Any](newCapacity)
          val newOps   = Array.ofDim[Int](newCapacity)
          System.arraycopy(queue, 0, newQueue, 0, last + 1)
          System.arraycopy(ops, 0, newOps, 0, last + 1)
          queue = newQueue
          ops = newOps
        }

        last += 1
        queue(last) = op
        ops(last) = opi
      }

      @tailrec def enqueue(l: Thunk): Unit = l match {
        case Pure(x)      => append(OP_PURE, x)
        case Delay(u)     => append(OP_DELAY, u)
        case Raise(e)     => append(OP_DELAY, (_: Any) => throw e)
        case Map(f, t)    => append(OP_MAP, f);    enqueue(t)
        case Bind(f, t)   => append(OP_BIND, f);   enqueue(t)
        case Handle(f, t) => append(OP_HANDLE, f); enqueue(t)
      }

      def reifiedStackPath[A](thunk: T[A]): Any = {
        last = -1
        enqueue(thunk)
        var result: Any    = null
        var exc: Throwable = null

        while (last >= 0) {
          try {
            while (last >= 0) {
              val q = queue(last)
              val op = ops(last)
              last -= 1

              if (exc != null) op match {
                case OP_HANDLE =>
                  enqueue(q.asInstanceOf[Throwable => Thunk](exc))
                  result = null
                  exc    = null
                case _ =>
              } else op match {
                case OP_PURE =>
                  result = q
                case OP_MAP =>
                  result = q.asInstanceOf[Any => Any](result)
                case OP_BIND =>
                  enqueue(q.asInstanceOf[Any => Thunk](result))
                  result = ()
                case OP_DELAY =>
                  result = q.asInstanceOf[Any => Any](result)
                case _ =>
              }
            }
          } catch {
            case NonFatal(e) =>
              while (last >= 0 && ops(last) != OP_HANDLE) last -= 1
              exc = e
          }
        }

        if (exc == null) result else throw exc
      }

      def happyPath[A](thunk: T[A], depth: Int): Any = {
        @tailrec def go(thunk: T[A]): Any = thunk match {
          case Pure(x)  => x
          case Delay(u) => u(null)
          case Raise(e) => throw e
          case Map(f, t) => f(happyPath(t, depth + 1))
          case Bind(f, t) => go(f(happyPath(t, depth + 1)))
          case Handle(f, t) =>
            var res: Any = null
            var exc: Throwable = null
            try { res = happyPath(t, depth + 1) } catch {
              case NonFatal(e) =>
                exc = e
            }
            if (exc == null) res
            else go(f(exc))
        }

        if (depth < HAPPY_PATH_DEPTH) go(thunk) else reifiedStackPath(thunk)
      }

      def run[A](thunk: T[A]): Either[Throwable, A] =
        try Right(happyPath(thunk, 0).asInstanceOf[A]) catch { case NonFatal(e) => Left(e) }
    }
  }

  val IOv3: IOv3Interface = new IOv3Implementation
  type IOv3[+A] = IOv3.T[A]
}
