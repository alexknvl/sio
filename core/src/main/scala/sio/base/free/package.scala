package sio.base.free

import scala.annotation.tailrec
import scala.util.control.NonFatal

object `package` {
  trait RealIOImpl {
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

  object RTS {
    @inline final val OP_PURE   = 0
    @inline final val OP_MAP    = 1
    @inline final val OP_BIND   = 2
    @inline final val OP_HANDLE = 3

    final val FAST_DEPTH = 128

    sealed abstract class Thunk
    final case class Pure(value: Any) extends Thunk
    final case class Map(f: Any => Any, tail: Thunk) extends Thunk
    final case class Bind(f: Any => Thunk, tail: Thunk) extends Thunk
    final case class Handle(f: Throwable => Thunk, tail: Thunk) extends Thunk
  }

  import RTS._

  final class RTS {
    var queue: Array[Any] = Array.ofDim[Any](16)
    var ops  : Array[Int] = Array.ofDim[Int](16)
    var last : Int = -1

    def append(opi: Int, op: Any): Unit = {
      if (last + 1 >= queue.length) {
        val newCapacity = queue.length * 3 / 2
        val newQueue = Array.ofDim[Any](newCapacity)
        val newOps = Array.ofDim[Int](newCapacity)
        System.arraycopy(queue, 0, newQueue, 0, last + 1)
        System.arraycopy(ops, 0, newOps, 0, last + 1)
        queue = newQueue
        ops = newOps
      }

      last += 1
      queue(last) = op
      ops(last) = opi
    }

    @inline @tailrec def enqueue(l: Any): Unit = l match {
      case Pure(x) =>
        append(OP_PURE, x)
      case Map(f, t) =>
        append(OP_MAP, f)
        enqueue(t)
      case Bind(f, t) =>
        append(OP_BIND, f)
        enqueue(t)
      case Handle(f, t) =>
        append(OP_HANDLE, f)
        enqueue(t)
    }

    def slowRun[A](action: Any): Any = {
      last = -1
      enqueue(action)
      var result: Any = ()
      var exc: Throwable = null

      while (last >= 0) {
        try {
          while (last >= 0) {
            val q = queue(last)
            val op = ops(last)
            last -= 1

            op match {
              case OP_PURE =>
                result = q
              case OP_HANDLE =>
                if (exc != null) {
                  enqueue(q.asInstanceOf[Throwable => Thunk](exc))
                  result = ()
                  exc = null
                }
              case OP_MAP =>
                result = q.asInstanceOf[Any => Any](result)
              case OP_BIND =>
                enqueue(q.asInstanceOf[Any => Thunk](result))
                result = ()
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

    def fastRun[A](action: Any, depth: Int): Any = {
      @tailrec def go(value: Any): Any = value match {
        case Pure(x) => x
        case Map(f, t) =>
          if (depth < FAST_DEPTH) f(fastRun(t, depth + 1))
          else f(slowRun[Any](t))
        case Bind(f, t) =>
          if (depth < FAST_DEPTH) go(f(fastRun(t, depth + 1)))
          else go(f(slowRun[Any](t)))
        case Handle(f, t) =>
          if (depth < FAST_DEPTH) {
            try fastRun(t, depth + 1) catch { case NonFatal(e) => f(e) }
          } else {
            try slowRun(t) catch { case NonFatal(e) => f(e) }
          }
      }

      go(action)
    }
  }

  type RealIO[+A] = RealIO.T[A]
  val RealIO: RealIOImpl = new RealIOImpl {
    type T[+A] = Thunk

    @inline final val unit: T[Unit] = Pure(())
    @inline final def pure[A](a: A): T[A] = Pure(a)
    @inline final def raise(e: Throwable): T[Nothing] =
      Map((_: Any) => throw e, null)
    @inline final def delay[A](a: Any => A): T[Nothing] =
      Map(a, null)

    @inline final def map[A, B](fa: T[A])(f: A => B): T[B] =
      Map(f.asInstanceOf[Any => Any], fa)
    @inline final def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B] =
      Bind(f.asInstanceOf[Any => Thunk], fa)
    @inline final def handle[A](fa: T[A])(f: Throwable => T[A]): T[A] =
      Handle(f.asInstanceOf[Throwable => Thunk], fa)

    final def run[A](action: T[A]): Either[Throwable, A] = {
      val rts = new RTS()
      try Right(rts.fastRun[A](action, 0).asInstanceOf[A]) catch { case NonFatal(e) => Left(e) }
    }
  }
}
