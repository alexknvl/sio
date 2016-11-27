package sio.core

import cats.data.EitherT
import cats.{MonadError, Monad}
import cats.syntax.either._
import sio.core.control.{RunInBase, MonadControlIO}

import scala.language.implicitConversions

final case class ST[S, A](unsafeUnwrap: dmz.RealIO[A]) {
  def map[B](f: A => B): ST[S, B] =
    new ST(unsafeUnwrap.map(f))
  def flatMap[B](f: A => ST[S, B]): ST[S, B] =
    new ST(unsafeUnwrap.flatMap(x => f(x).unsafeUnwrap))
  def handleErrorWith(f: Throwable => ST[S, A]): ST[S, A] =
    new ST(unsafeUnwrap.handleErrorWith(x => f(x).unsafeUnwrap))

  /**
    * Handle any error, by mapping it to an `A` value.
    *
    * @see [[handleErrorWith]] to map to an `IO[A]` value instead of simply an
    * `A` value.
    * @see [[recover]] to only recover from certain errors.
    */
  def handleError(f: Throwable => A): ST[S, A] =
    handleErrorWith(f andThen ST.pure[S, A])

  /**
    * Turns a successful value into an error if it does not satisfy a given predicate.
    */
  def ensure(error: => Throwable)(predicate: A => Boolean): ST[S, A] =
    flatMap(a => if (predicate(a)) ST.pure(a) else ST.raiseError(error))

  /**
    * Handle errors by turning them into [[scala.util.Either]] values.
    *
    * If there is no error, then an `scala.util.Right` value will be returned instead.
    *
    * All non-fatal errors should be handled by this method.
    */
  def attempt: ST[S, Either[Throwable, A]] =
    map(Either.right[Throwable, A]).handleErrorWith(e => ST.pure(Either.left[Throwable, A](e)))

  /**
    * Similar to [[attempt]], but wraps the result in a [[cats.data.EitherT]] for
    * convenience.
    */
  def attemptT: EitherT[ST[S, ?], Throwable, A] = EitherT[ST[S, ?], Throwable, A](attempt)

  /**
    * Recover from certain errors by mapping them to an `A` value.
    *
    * @see [[handleError]] to handle any/all errors.
    * @see [[recoverWith]] to recover from certain errors by mapping them to
    * `IO[A]` values.
    */
  def recover(pf: PartialFunction[Throwable, A]): ST[S, A] =
    handleErrorWith(e => (pf andThen ST.pure[S, A]) applyOrElse(e, ST.raiseError[S, A]))

  /**
    * Recover from certain errors by mapping them to an `IO[A]` value.
    *
    * @see [[handleErrorWith]] to handle any/all errors.
    * @see [[recover]] to recover from certain errors by mapping them to `A`
    * values.
    */
  def recoverWith(pf: PartialFunction[Throwable, ST[S, A]]): ST[S, A] =
    handleErrorWith(e => pf applyOrElse(e, ST.raiseError))

  /**
    * Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  def >>[B](next: ST[S, B]): ST[S, B] = flatMap(_ => next)

  /**
    * Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  def *>[B](next: ST[S, B]): ST[S, B] = flatMap(_ => next)

  /**
    * Sequence actions, discarding the value of the second argument.
    */
  def <*[B](next: ST[S, B]): ST[S, A] = flatMap(a => next.map(_ => a))

  /**
    * forever repeats the action infinitely.
    *
    * @return does not return under normal circumstances
    */
  def forever: ST[S, Nothing] = flatMap[Nothing](_ => forever)

  /**
    * Like "finally", but only performs the final action if there was an exception.
    */
  def onException[B](action: ST[S, B]): ST[S, A] =
    handleErrorWith(e => action.flatMap(_ => ST.raiseError[S, A](e)))

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an exception.
    * All exceptions are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => ST[S, B])(during: A => ST[S, C]): ST[S, C] =
    flatMap(a => during(a).onException(after(a)) <* after(a))

  /**
    * Like "bracket", but takes only a computation to run afterward. Generalizes "finally".
    */
  def ensuring[B](sequel: ST[S, B]): ST[S, A] =
    onException(sequel) <* sequel

  /**
    * A variant of "bracket" where the return value of this computation is not needed.
    */
  def bracket_[B, C](after: ST[S, B])(during: ST[S, C]): ST[S, C] =
    flatMap(a => during.onException(after) <* after)

  /**
    * A variant of "bracket" that performs the final action only if there was an error.
    */
  def bracketOnError[B, C](after: A => ST[S, B])(during: A => ST[S, C]): ST[S, C] =
    flatMap(a => during(a).onException(after(a)))
}

object ST {
  def attempt[A](f: Forall[ST[?, A]]): Either[Throwable, A] = f.apply.unsafeUnwrap.attempt()
  def unsafeRun[A](f: Forall[ST[?, A]]): A = f.apply.unsafeUnwrap.run()

  def unsafeCapture[S, A](a: => A): ST[S, A] = new ST(dmz.capture(a))

  def unit[S]: ST[S, Unit] = ST(dmz.unit)
  def pure[S, A](x: A): ST[S, A] = ST(dmz.pure(x))
  def raiseError[S, A](e: Throwable): ST[S, A] = ST(dmz.raiseError(e))

  def trace[S](s: String): ST[S, Unit] = unsafeCapture { System.err.println(s) }
}