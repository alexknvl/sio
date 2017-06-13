package sio.core.syntax

import java.util.concurrent.Callable

import leibniz.===
import sio.core._
import cats.data.EitherT
import cats.syntax.either._
import cats.instances.either._
import sio.base.RunInBase

final class STSyntaxOps[S, A](val value: ST[S, A]) extends AnyVal {
  def map[B](f: A => B): ST[S, B] =
    ST.map[S, A, B](value)(f)

  def flatMap[B](f: A => ST[S, B]): ST[S, B] =
    ST.flatMap[S, A, B](value)(f)

  def handleErrorWith(f: Throwable => ST[S, A]): ST[S, A] =
    ST.handleErrorWith[S, A](value)(f)

  def liftMap[B](f: A => Impure[B]): ST[S, B] =
    ST.flatMap[S, A, B](value)(a => ST.unsafeCapture(f(a)))

  /** This method allows you to convert an `ST` computation into a callback that
    * can be passed to impure methods. For this to be safe, the impure method
    * taking a callback must not let it escape outside the `ST` monad.
    *
    * @see [[ST.unsafeCallback]]
    * @see [[asRunnable]]
    * @see [[asCallable]]
    */
  def asCallback(implicit ev: S === RW): ST[RW, () => Impure[A]] =
    ev.subst[ST[?, () => Impure[A]]](ST.unsafeCallback0[S, A](value))

  /** This method allows you to convert an `ST` computation into a [[Runnable]] that
    * can be passed to impure methods. For this to be safe, the impure method
    * taking a `Runnable` must not let it escape outside the `ST` monad.
    *
    * @see [[asCallback]]
    * @see [[asCallable]]
    */
  def asRunnable(implicit ev: S === RW): ST[RW, Runnable] =
    ST.map(asCallback(ev))((f: () => A) => new Runnable { override def run(): Unit = f() })

  /** This method allows you to convert an `ST` computation into a [[Callable]] that
    * can be passed to impure methods. For this to be safe, the impure method
    * taking a `Callable` must not let it escape outside the `ST` monad.
    *
    * @see [[asCallback]]
    * @see [[asRunnable]]
    */
  def asCallable(implicit ev: S === RW): ST[RW, Callable[A]] =
    ST.map(asCallback(ev))((f: () => A) => new Callable[A] { override def call(): A = f() })

  /** Handle any error, by mapping it to an `A` value.
    *
    * @see [[handleErrorWith]] to map to an `IO[A]` value instead of simply an
    * `A` value.
    * @see [[recover]] to only recover from certain errors.
    */
  def handleError(f: Throwable => A): ST[S, A] =
    handleErrorWith(f andThen ST.pure[S, A])

  /** Turns a successful value into an error if it does not satisfy a given predicate.
    */
  def ensure(error: => Throwable)(predicate: A => Boolean): ST[S, A] =
    ST.flatMap[S, A, A](value)(a => if (predicate(a)) ST.pure(a) else ST.raise[S](error))

  /** Handle errors by turning them into [[scala.util.Either]] values.
    *
    * If there is no error, then an `scala.util.Right` value will be returned instead.
    *
    * All non-fatal errors should be handled by this method.
    */
  def attempt: ST[S, Either[Throwable, A]] = {
    val r = map(Either.right[Throwable, A])
    ST.handleErrorWith[S, Either[Throwable, A]](r)(e => ST.pure(Either.left[Throwable, A](e)))
  }


  /** Similar to [[attempt]], but wraps the result in a [[cats.data.EitherT]] for
    * convenience.
    */
  def attemptT: EitherT[ST[S, ?], Throwable, A] = EitherT[ST[S, ?], Throwable, A](attempt)

  /** Recover from certain errors by mapping them to an `A` value.
    *
    * @see [[handleError]] to handle any/all errors.
    * @see [[recoverWith]] to recover from certain errors by mapping them to
    * `IO[A]` values.
    */
  def recover(pf: PartialFunction[Throwable, A]): ST[S, A] =
    handleErrorWith(e => (pf andThen ST.pure[S, A]) applyOrElse(e, ST.raise[S]))

  /** Recover from certain errors by mapping them to an `IO[A]` value.
    *
    * @see [[handleErrorWith]] to handle any/all errors.
    * @see [[recover]] to recover from certain errors by mapping them to `A`
    * values.
    */
  def recoverWith(pf: PartialFunction[Throwable, ST[S, A]]): ST[S, A] =
    handleErrorWith(e => pf applyOrElse(e, ST.raise[S]))

  /** Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  def >>[B](next: ST[S, B]): ST[S, B] = ST.flatMap(value)((_: A) => next)

  /** Sequentially compose two actions, discarding any value produced by the first,
    * like sequencing operators (such as the semicolon) in imperative languages.
    */
  def *>[B](next: ST[S, B]): ST[S, B] = ST.flatMap(value)((_: A) => next)

  /** Sequence actions, discarding the value of the second argument.
    */
  def <*[B](next: ST[S, B]): ST[S, A] = ST.flatMap(value)((a: A) => ST.map(next)((_: B) => a))

  /** Repeats the action infinitely.
    */
  def forever: ST[S, Nothing] = flatMap[Nothing](_ => forever)

  private[this] final def finally1[A, B](body: ST[S, A], action: ST[S, B]): ST[S, A] =
    ST.handleErrorWith(body)((e: Throwable) => ST.flatMap(action)((_: B) => ST.raise[S](e)))
  private[this] final def left[A, B](l: ST[S, A], r: ST[S, B]): ST[S, A] =
    ST.flatMap(l)((a: A) => ST.map(r)((_: B) => a))

  /** Like "finally", but only performs the final action if there was an exception.
    */
  def onException[B](action: ST[S, B]): ST[S, A] = finally1[A, B](value, action)

  /** Applies the "during" action, calling "after" regardless of whether there was an exception.
    * All exceptions are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => ST[S, B])(during: A => ST[S, C]): ST[S, C] =
    ST.flatMap(value)((a: A) => left(finally1(during(a), after(a)), after(a)))

  /** Like "bracket", but takes only a computation to run afterward. Generalizes "finally".
    */
  def ensuring[B](sequel: ST[S, B]): ST[S, A] =
    left(onException(sequel), sequel)

  /** A variant of "bracket" where the return value of this computation is not needed.
    */
  def bracket_[B, C](after: ST[S, B])(during: ST[S, C]): ST[S, C] =
    flatMap(a => left(finally1(during, after), after))

  /** A variant of "bracket" that performs the final action only if there was an error.
    */
  def bracketOnError[B, C](after: A => ST[S, B])(during: A => ST[S, C]): ST[S, C] =
    flatMap(a => finally1(during(a), after(a)))

  /**
    * Lift this action into a given IO-like monad.
    */
  def liftIO[F[_]](implicit ev: S === RW, F: LiftIO[F]): F[A] =
    F.liftIO(ev.subst[ST[?, A]](value))

  def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit ev: S === RW, M: MonadControlIO[M]): M[B] =
    M.control { (runInIO: RunInBase[M, IO]) =>
      new STSyntaxOps(ev.subst[ST[?, A]](value)).bracket(after)(during andThen runInIO.apply)
    }
}
trait STSyntax {
  implicit def toSTSyntaxOps[S, A](st: ST[S, A]): STSyntaxOps[S, A] =
    new STSyntaxOps[S, A](st)
}
