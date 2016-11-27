package sio.core

import java.util.concurrent.Callable

import scala.util.Try
import cats.syntax.either._

object IO {
  def apply[A](f: => A): IO[A] = ST.unsafeCapture(f)

  val unit: IO[Unit] = ST.unit
  def pure[A](x: A): IO[A] = ST.pure(x)
  def raiseError[A](e: Throwable): IO[A] = ST.raiseError(e)
  def trace(s: String): IO[Unit] = ST.trace(s)

  /**
    * This is the "back door" into the IO monad, allowing IO computation
    * to be performed at any time. For this to be safe, the IO computation
    * should be free of side effects and independent of its environment.
    *
    * @return a value of type `A`.
    * @see [[unsafeRun]] to rethrow all exceptions.
    */
  def unsafeAttempt[A](io: IO[A]): Either[Throwable, A] = io.unsafeUnwrap.attempt()
  def unsafeTry[A](io: IO[A]): Try[A] = io.unsafeUnwrap.attempt().toTry
  def unsafeRun[A](io: IO[A]): A = io.unsafeUnwrap.run()
  def unsafeRunnable[A](io: IO[A]): Runnable = new Runnable { def run(): Unit = io.unsafeUnwrap.run() }
  def unsafeCallable[A](io: IO[A]): Callable[A] = new Callable[A] { def call(): A = io.unsafeUnwrap.run() }
}