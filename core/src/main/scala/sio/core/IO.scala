package sio.core

import cats.data.Xor

sealed abstract class IO[A] extends Product with Serializable {
  def map[B](f: A => B): IO[B]
  def flatMap[B](f: A => IO[B]): IO[B]
  def handleErrorWith(f: Throwable => IO[A]): IO[A]

  def unsafeAttempt(): Xor[Throwable, A]
  def unsafeRun(): A

  def ensure(error: => Throwable)(predicate: A => Boolean): IO[A] =
    flatMap(a => if (predicate(a)) IO.pure(a) else IO.raiseError(error))

  def attempt: IO[Xor[Throwable, A]] =
    map(Xor.right[Throwable, A]).handleErrorWith(e => IO.pure(Xor.left(e)))

  def recover(pf: PartialFunction[Throwable, A]): IO[A] =
    handleErrorWith(e => (pf andThen IO.pure) applyOrElse(e, IO.raiseError))

  def recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    handleErrorWith(e => pf applyOrElse(e, IO.raiseError))
}

@SuppressWarnings(Array("org.wartremover.warts.LeakingSealed"))
private [core] abstract class ForwarderIO[A] extends IO[A]

object IO {
  def apply[A](f: => A): IO[A] = dmz.capture(f)
  def pure[A](x: A): IO[A] = dmz.pure(x)
  def raiseError[A](e: Throwable): IO[A] = dmz.raiseError(e)
}