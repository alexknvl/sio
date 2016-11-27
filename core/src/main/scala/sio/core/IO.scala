package sio.core

import java.util.concurrent.Callable

import cats.data.EitherT
import scala.util.Try
import cats.syntax.all._

final case class IO[A](unsafeUnwrap: dmz.RealIO[A]) {
  /**
    * This is the "back door" into the IO monad, allowing IO computation
    * to be performed at any time. For this to be safe, the IO computation
    * should be free of side effects and independent of its environment.
    *
    * @return a value of type `A`.
    * @see [[unsafeRun]] to rethrow all exceptions.
    */
  def unsafeAttempt(): Either[Throwable, A] = unsafeUnwrap.attempt()
  def unsafeTry(): Try[A] = unsafeAttempt().toTry
  def unsafeRun(): A = unsafeUnwrap.run()
  def unsafeRunnable: Runnable = new Runnable { def run(): Unit = unsafeRun() }
  def unsafeCallable: Callable[A] = new Callable[A] { def call(): A = unsafeRun() }

  /**
    * Lift this action into a given IO-like monad.
    */
  def liftIO[F[_]](implicit F: LiftIO[F]): F[A] = F.liftIO(this)

  def map[B](f: A => B): IO[B] =
    new IO(unsafeUnwrap.map(f))
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO(unsafeUnwrap.flatMap(x => f(x).unsafeUnwrap))
  def handleErrorWith(f: Throwable => IO[A]): IO[A] =
    new IO(unsafeUnwrap.handleErrorWith(x => f(x).unsafeUnwrap))

  /**
    * Handle any error, by mapping it to an `A` value.
    *
    * @see [[handleErrorWith]] to map to an `IO[A]` value instead of simply an
    * `A` value.
    * @see [[recover]] to only recover from certain errors.
    */
  def handleError(f: Throwable => A): IO[A] =
    handleErrorWith(f andThen IO.pure)

  /**
    * Turns a successful value into an error if it does not satisfy a given predicate.
    */
  def ensure(error: => Throwable)(predicate: A => Boolean): IO[A] =
    flatMap(a => if (predicate(a)) IO.pure(a) else IO.raiseError(error))

  /**
    * Handle errors by turning them into [[scala.util.Either]] values.
    *
    * If there is no error, then an `scala.util.Right` value will be returned instead.
    *
    * All non-fatal errors should be handled by this method.
    */
  def attempt: IO[Either[Throwable, A]] =
    map(Either.right[Throwable, A]).handleErrorWith(e => IO.pure(Either.left[Throwable, A](e)))

  /**
    * Similar to [[attempt]], but wraps the result in a [[cats.data.EitherT]] for
    * convenience.
    */
  def attemptT: EitherT[IO, Throwable, A] = EitherT(attempt)

  /**
    * Recover from certain errors by mapping them to an `A` value.
    *
    * @see [[handleError]] to handle any/all errors.
    * @see [[recoverWith]] to recover from certain errors by mapping them to
    * `IO[A]` values.
    */
  def recover(pf: PartialFunction[Throwable, A]): IO[A] =
    handleErrorWith(e => (pf andThen IO.pure) applyOrElse(e, IO.raiseError))

  /**
    * Recover from certain errors by mapping them to an `IO[A]` value.
    *
    * @see [[handleErrorWith]] to handle any/all errors.
    * @see [[recover]] to recover from certain errors by mapping them to `A`
    * values.
    */
  def recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    handleErrorWith(e => pf applyOrElse(e, IO.raiseError))

  /**
    * Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  def >>[B](next: IO[B]): IO[B] = flatMap(_ => next)

  /**
    * Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  def *>[B](next: IO[B]): IO[B] = flatMap(_ => next)

  /**
    * Sequence actions, discarding the value of the second argument.
    */
  def <*[B](next: IO[B]): IO[A] = flatMap(a => next.map(_ => a))

  /**
    * forever repeats the action infinitely.
    *
    * @return does not return under normal circumstances
    */
  def forever: IO[Nothing] = flatMap[Nothing](_ => forever)

  /**
    * Like "finally", but only performs the final action if there was an exception.
    */
  def onException[B](action: IO[B]): IO[A] =
    handleErrorWith(e => action.flatMap(_ => IO.raiseError[A](e)))

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an exception.
    * All exceptions are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] =
    flatMap(a => during(a).onException(after(a)) <* after(a))

  /**
    * Like "bracket", but takes only a computation to run afterward. Generalizes "finally".
    */
  def ensuring[B](sequel: IO[B]): IO[A] =
    onException(sequel) <* sequel

  /**
    * A variant of "bracket" where the return value of this computation is not needed.
    */
  def bracket_[B, C](after: IO[B])(during: IO[C]): IO[C] =
    flatMap(a => during.onException(after) <* after)

  /**
    * A variant of "bracket" that performs the final action only if there was an error.
    */
  def bracketOnError[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] =
    flatMap(a => during(a).onException(after(a)))
}

object IO {
  val unit: IO[Unit] = new IO(dmz.unit)

  def apply[A](f: => A): IO[A] = new IO(dmz.capture(f))
  def pure[A](x: A): IO[A] = new IO(dmz.pure(x))
  def raiseError[A](e: Throwable): IO[A] = new IO(dmz.raiseError(e))
  def trace(s: String): IO[Unit] = IO { System.err.println(s) }

  implicit val instance: MonadIO[IO] = new MonadIO[IO] {
    override def pure[A](x: A): IO[A] =
      IO.pure(x)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] =
      IO(fa.unsafeUnwrap.map(f))
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      IO(fa.unsafeUnwrap.flatMap(a => f(a).unsafeUnwrap))
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] =
      IO(dmz.tailRecM(a)(a => f(a).unsafeUnwrap))

    override def raiseError[A](e: Throwable): IO[A] =
      IO.raiseError(e)
    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      IO(fa.unsafeUnwrap.handleErrorWith(t => f(t).unsafeUnwrap))

    override def capture[A](a: => A): IO[A] = IO(a)
    override def liftIO[A](a: IO[A]): IO[A] = a
  }
}