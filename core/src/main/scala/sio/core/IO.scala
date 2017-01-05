package sio.core

import cats.~>
import cats.syntax.either._

object IO {
  /** Creates an IO action that produces a unit value without performing
    * any side-effects.
    */
  val unit: IO[Unit] = ST.unit

  /** Creates an IO action that produces a specific value without performing
    * any side-effects.
    */
  def pure[A](x: A): IO[A] = ST.pure(x)

  /** Creates a failed IO action without any side-effects.
    */
  def raise[A](e: Throwable): IO[A] = ST.raise(e)

  /** This method allows you to lift arbitrary IO actions into IO monad.
    * For this to be safe, the action should only perform local side-effects.
    *
    * @see apply
    */
  def capture[A](f: => Impure[A]): IO[A] = ST.unsafeCapture(f)

  /** This method allows you to lift arbitrary IO actions into IO monad.
    * For this to be safe, the action should only perform local side-effects.
    *
    * @see capture
    */
  def apply[A](f: => Impure[A]): IO[A] = capture(f)

  /** This method allows you to convert an ST computation into a callback that
    * can be passed to impure methods. For this to be safe, the impure method
    * taking a callback must not let it escape outside the ST monad.
    */
  def callback[A, B](run: A => IO[B]): IO[A => Impure[B]] = ST.unsafeCallback(run)

  /** Prints a message to the standard error output. This function is intended
    * only for debugging and it is neither referentially transparent nor IO-free.
    * It can be useful for investigating bugs or performance problems.
    * Should not be used in production code.
    */
  def trace(s: String): IO[Unit] = IO { System.err.println(s) }

  def mutable[A](a: A): IOMutable[A] = new Mutable(a)

  private[this] val ioInterpreter: IOOp[World.Real, ?] ~> Either[Throwable, ?] =
    new (IOOp[World.Real, ?] ~> Either[Throwable, ?]) {
      override def apply[B](op: IOOp[World.Real, B]): Either[Throwable, B] = op match {
        case IOOp.Lift(f) =>
          Either.catchNonFatal(f())
        case unlift: IOOp.Unlift[World.Real, f, t] =>
          Either.right[Throwable, B]((x: f) => unsafeRun(unlift.run(x)))
      }
    }

  /**
    * This is the "back door" into the IO monad, allowing IO computation
    * to be performed at any time. For this to be safe, the IO computation
    * should be free of side effects and independent of its environment.
    *
    * @return a value of type `Either[Throwable, A]`.
    * @see unsafeRun
    */
  def unsafeAttempt[A](io: IO[A]): Impure[Either[Throwable, A]] =
    io.value.run(Either.right[Throwable, Unit](()), ioInterpreter)

  /**
    * This is the "back door" into the IO monad, allowing IO computation
    * to be performed at any time. For this to be safe, the IO computation
    * should be free of side effects and independent of its environment.
    *
    * @return a value of type `A`.
    * @see unsafeAttempt
    */
  def unsafeRun[A](io: IO[A]): Impure[A] =
    unsafeAttempt(io).fold(e => throw e, identity)
}