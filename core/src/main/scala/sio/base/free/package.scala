package sio.base.free

import scala.annotation.tailrec
import scala.util.control.NonFatal

object `package` {
  trait RealIOImpl {
    type T[+A]

    def unit: T[Unit]
    def pure[A](a: A): T[A]
    def raise(e: Throwable): T[Nothing]

    def map[A, B](fa: T[A])(f: A => B): T[B]
    def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B]
    def handle[A](fa: T[A])(f: Throwable => T[A]): T[A]

    def run[A](action: T[A]): Either[Throwable, A]
  }

  object RTS {
    @inline final val OP_PURE   = 0
    @inline final val OP_RAISE  = 1
    @inline final val OP_MAP    = 2
    @inline final val OP_BIND   = 3
    @inline final val OP_HANDLE = 4

    final val FAST_DEPTH = 128

    sealed abstract class Thunk
    final case class Raise[A](e: Throwable) extends Thunk
    final case class Map[A](f: Any => Any, tail: Any) extends Thunk
    final case class Bind[A](f: Any => Thunk, tail: Any) extends Thunk
    final case class Handle[A](f: Throwable => Any, tail: Any) extends Thunk
  }

  import RTS._

  final class RTS {
    var queue = Array.ofDim[Any](16)
    var ops   = Array.ofDim[Int](16)
    var last  = -1

    @inline def append(opi: Int, op: Any): Unit = {
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

    @tailrec def enqueue(l: Any): Unit = l match {
      case Raise(x) =>
        append(OP_RAISE, x)
      case Map(f, t) =>
        append(OP_MAP, f)
        enqueue(t)
      case Bind(f, t) =>
        append(OP_BIND, f)
        enqueue(t)
      case Handle(f, t) =>
        append(OP_HANDLE, f)
        enqueue(t)
      case x =>
        append(OP_PURE, x)
    }

    final def slowRun[A](action: Any): Any = {
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
              case OP_RAISE =>
                throw q.asInstanceOf[Throwable]
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

      if (exc == null) result
      else throw exc
    }

    final def fastRun[A](action: Any, depth: Int): Any = {
      @tailrec def go(value: Any): Any = value match {
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
        case Raise(x) => throw x
        case x => x
      }

      go(action)
    }
  }

  type RealIO[+A] = RealIO.T[A]
  val RealIO: RealIOImpl = new RealIOImpl {
    type T[+A] = Any

    @inline final val unit: T[Unit] = ()
    @inline final def pure[A](a: A): T[A] = a // Pure(a)
    @inline final def raise(e: Throwable): T[Nothing] = Raise(e)

    @inline final def map[A, B](fa: T[A])(f: A => B): T[B] =
      new Map(f.asInstanceOf[Any => Any], fa)
    @inline final def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B] =
      new Bind(f.asInstanceOf[Any => Thunk], fa)
    @inline final def handle[A](fa: T[A])(f: Throwable => T[A]): T[A] =
      new Handle(f.asInstanceOf[Throwable => Any], fa)

    final def run[A](action: T[A]): Either[Throwable, A] = {
      val rts = new RTS()
      try Right(rts.fastRun[A](action, 0).asInstanceOf[A]) catch { case NonFatal(e) => Left(e) }
    }
  }
}
