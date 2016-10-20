package sio.core

import cats.data.Xor

sealed abstract class IO[A] extends Product with Serializable {
  def map[B](f: A => B): IO[B]
  def flatMap[B](f: A => IO[B]): IO[B]
  def handleErrorWith(f: Throwable => IO[A]): IO[A]

  def unsafeAttempt(): Xor[Throwable, A]
  def unsafeRun(): A

  final def ensure(error: => Throwable)(predicate: A => Boolean): IO[A] =
    flatMap(a => if (predicate(a)) IO.pure(a) else IO.raiseError(error))

  final def attempt: IO[Xor[Throwable, A]] =
    map(Xor.right[Throwable, A]).handleErrorWith(e => IO.pure(Xor.left[Throwable, A](e)))

  final def recover(pf: PartialFunction[Throwable, A]): IO[A] =
    handleErrorWith(e => (pf andThen IO.pure) applyOrElse(e, IO.raiseError))

  final def recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    handleErrorWith(e => pf applyOrElse(e, IO.raiseError))

  final def >>[B](next: IO[B]): IO[B] = flatMap(_ => next)
  final def *>[B](next: IO[B]): IO[B] = flatMap(_ => next)
  final def <*[B](next: IO[B]): IO[A] = flatMap(a => next.map(_ => a))

  final def forever: IO[Unit] = flatMap[Unit](_ => forever)

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