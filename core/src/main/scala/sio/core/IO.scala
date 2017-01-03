package sio.core

import cats.~>
import cats.syntax.either._

object IO {
  val unit: IO[Unit] = ST.unit
  def pure[A](x: A): IO[A] = ST.pure(x)
  def raiseError[A](e: Throwable): IO[A] = ST.raiseError(e)
  def capture[A](f: => Impure[A]): IO[A] = ST.unsafeCapture(f)

  def apply[A](f: => Impure[A]): IO[A] = capture(f)

  def mutable[A](a: A): IOMutable[A] = new Mutable(a)
  def trace(s: String): IO[Unit] = IO { System.err.println(s) }

  private[this] val ioInterpreter: Op[World.Real, ?] ~> Either[Throwable, ?] =
    new (Op[World.Real, ?] ~> Either[Throwable, ?]) {
      override def apply[B](op: Op[World.Real, B]): Either[Throwable, B] = op match {
        case Op.Lift(f) =>
          Either.catchNonFatal(f())
        case Op.Unlift(fa) =>
          Either.right(() => unsafeRun(fa).fold(e => throw e, _ => ()))
      }
    }

  /**
    * This is the "back door" into the IO monad, allowing IO computation
    * to be performed at any time. For this to be safe, the IO computation
    * should be free of side effects and independent of its environment.
    *
    * @return a value of type `A`.
    */
  def unsafeRun[A](io: IO[A]): Impure[Either[Throwable, A]] =
    io.value.run(Right(()), ioInterpreter)
}