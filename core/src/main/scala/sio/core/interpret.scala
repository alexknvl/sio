package sio.core

import cats.data.Xor

import sio.core.internal._

object interpret {
  object Folder extends FoldUnfold[IO.Op, Xor[Throwable, ?]] {
    val xorUnit = Xor.right[Throwable, Unit](())

    override def apply[A, B](acc: Xor[Throwable, A], next: IO.Op[A, B]): Result[B] =
      acc match {
        case Xor.Left(e) => next match {
          case IO.Op.Handle(f) =>
            // FIXME: Once scala supports GADT type refinement, it should be possible to remove casts here.
            Xor.catchNonFatal(f(e).asInstanceOf[IO[A]]).fold(
              e2 => none(Xor.left(e)),
              io => many(xorUnit, TypeAligned[IO.Op, Unit, B](io.ops.repr)))
          case _ => none(Xor.left(e))
        }
        case Xor.Right(a) => next match {
          case IO.Op.Map(f) =>
            Xor.catchNonFatal(f(a)).fold(
              e => none(Xor.left(e)),
              b => none(Xor.right(b)))
          case IO.Op.Bind(f) =>
            Xor.catchNonFatal(f(a)).fold(
              e => none(Xor.left(e)),
              b => many(xorUnit, b.ops))
          case IO.Op.Handle(_) =>
            // FIXME: Once scala supports GADT type refinement, it should be possible to remove casts here.
            none(Xor.right(a.asInstanceOf[B]))
        }
      }
  }

  def unsafeAttemptIO[A](io: IO[A]): Throwable Xor A =
    io.ops.foldUnfold[Xor[Throwable, ?]](Xor.right(()), Folder)
  def unsafePerformIO[A](io: IO[A]): A =
    unsafeAttemptIO(io).fold(e => throw e, a => a)
}
