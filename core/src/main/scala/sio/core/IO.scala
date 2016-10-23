package sio.core

import cats.data.EitherT
import cats.syntax.either._

sealed abstract class IO[A] extends Product with Serializable {
  def unsafeAttempt(): Either[Throwable, A]
  def unsafeRun(): A

  def map[B](f: A => B): IO[B]
  def flatMap[B](f: A => IO[B]): IO[B]

  /**
    * Handle any error, potentially recovering from it, by mapping it to an
    * `F[A]` value.
    *
    * @see [[handleError]] to handle any error by simply mapping it to an `A`
    * value instead of an `F[A]`.
    *
    * @see [[recoverWith]] to recover from only certain errors.
    */
  def handleErrorWith(f: Throwable => IO[A]): IO[A]

  /**
    * Handle any error, by mapping it to an `A` value.
    *
    * @see [[handleErrorWith]] to map to an `F[A]` value instead of simply an
    * `A` value.
    *
    * @see [[recover]] to only recover from certain errors.
    */
  final def handleError(f: Throwable => A): IO[A] =
    handleErrorWith(f andThen IO.pure)

  /**
    * Turns a successful value into an error if it does not satisfy a given predicate.
    */
  final def ensure(error: => Throwable)(predicate: A => Boolean): IO[A] =
    flatMap(a => if (predicate(a)) IO.pure(a) else IO.raiseError(error))

  /**
    * Handle errors by turning them into [[scala.util.Either]] values.
    *
    * If there is no error, then an `scala.util.Right` value will be returned instead.
    *
    * All non-fatal errors should be handled by this method.
    */
  final def attempt: IO[Either[Throwable, A]] =
    map(Either.right[Throwable, A]).handleErrorWith(e => IO.pure(Either.left[Throwable, A](e)))

  /**
    * Similar to [[attempt]], but wraps the result in a [[cats.data.EitherT]] for
    * convenience.
    */
  final def attemptT: EitherT[IO, Throwable, A] = EitherT(attempt)

  /**
    * Recover from certain errors by mapping them to an `A` value.
    *
    * @see [[handleError]] to handle any/all errors.
    *
    * @see [[recoverWith]] to recover from certain errors by mapping them to
    * `F[A]` values.
    */
  final def recover(pf: PartialFunction[Throwable, A]): IO[A] =
    handleErrorWith(e => (pf andThen IO.pure) applyOrElse(e, IO.raiseError))

  /**
    * Recover from certain errors by mapping them to an `F[A]` value.
    *
    * @see [[handleErrorWith]] to handle any/all errors.
    *
    * @see [[recover]] to recover from certain errors by mapping them to `A`
    * values.
    */
  final def recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    handleErrorWith(e => pf applyOrElse(e, IO.raiseError))

  /**
    * Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  final def >>[B](next: IO[B]): IO[B] = flatMap(_ => next)

  /**
    * Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  final def *>[B](next: IO[B]): IO[B] = flatMap(_ => next)

  /**
    * Sequence actions, discarding the value of the second argument.
    */
  final def <*[B](next: IO[B]): IO[A] = flatMap(a => next.map(_ => a))

  /**
    * forever repeats the action infinitely.
    * @return does not return under normal circumstances
    */
  final def forever: IO[Nothing] = flatMap[Nothing](_ => forever)

  /**
    * Like "finally", but only performs the final action if there was an exception.
    */
  final def onException[B](action: IO[B]): IO[A] =
    handleErrorWith(e => action.flatMap(_ => IO.raiseError[A](e)))

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an exception.
    * All exceptions are rethrown. Generalizes try/finally.
    */
  final def bracket[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] =
    flatMap(a => during(a).onException(after(a)) <* after(a))

  /**
    * Like "bracket", but takes only a computation to run afterward. Generalizes "finally".
    */
  final def ensuring[B](sequel: IO[B]): IO[A] =
    onException(sequel) <* sequel

  /**
    * A variant of "bracket" where the return value of this computation is not needed.
    */
  final def bracket_[B, C](after: IO[B])(during: IO[C]): IO[C] =
    flatMap(a => during.onException(after) <* after)

  /**
    * A variant of "bracket" that performs the final action only if there was an error.
    */
  final def bracketOnError[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] =
    flatMap(a => during(a).onException(after(a)))

  final def liftIO[F[_]](implicit F: LiftIO[F]): F[A] = F.liftIO(this)
}

@SuppressWarnings(Array("org.wartremover.warts.LeakingSealed"))
private [core] abstract class ForwarderIO[A] extends IO[A]

object IO {
  val unit: IO[Unit] = dmz.unit

  def apply[A](f: => A): IO[A] = dmz.capture(f)
  def pure[A](x: A): IO[A] = dmz.pure(x)
  def raiseError[A](e: Throwable): IO[A] = dmz.raiseError(e)

  def trace(s: String): IO[Unit] = IO { System.err.println(s) }

  implicit val instance: MonadIO[IO] = new MonadIO[IO] {
    override def pure[A](x: A): IO[A] = IO.pure(x)
    override def capture[A](a: => A): IO[A] = IO(a)

    override def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = fa.handleErrorWith(f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] = defaultTailRecM(a)(f)

    override def liftIO[A](a: IO[A]): IO[A] = a
  }
}