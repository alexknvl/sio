package sio

import java.util.concurrent.Callable

import sio.core.control.{RunInBase, MonadControlIO}

import scala.util.Try
import cats.syntax.all._

package object core {
  final abstract class RealWorld private ()
  type IO[A] = ST[RealWorld, A]

  implicit class IOExtra[A] (val value: IO[A]) extends AnyVal {
    /**
      * This is the "back door" into the IO monad, allowing IO computation
      * to be performed at any time. For this to be safe, the IO computation
      * should be free of side effects and independent of its environment.
      *
      * @return a value of type `A`.
      * @see [[unsafeRun]] to rethrow all exceptions.
      */
    def unsafeAttempt(): Either[Throwable, A] = value.unsafeUnwrap.attempt()
    def unsafeTry(): Try[A] = value.unsafeAttempt().toTry
    def unsafeRun(): A = value.unsafeUnwrap.run()
    def unsafeRunnable: Runnable = new Runnable { def run(): Unit = unsafeRun() }
    def unsafeCallable: Callable[A] = new Callable[A] { def call(): A = unsafeRun() }

    /**
      * Lift this action into a given IO-like monad.
      */
    def liftIO[F[_]](implicit F: LiftIO[F]): F[A] = F.liftIO(value)

    def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit M: MonadControlIO[M]): M[B] =
      M.control((runInIO: RunInBase[M, IO]) => value.bracket(after)(during andThen runInIO.apply))
  }

  type STRef[S, A] = Ref[S, A]
  type IORef[A] = Ref[RealWorld, A]

  def newSTRef[S, A](a: A): ST[S, Ref[S, A]] = ST.unsafeCapture { new Ref[S, A](a) }
  def newIORef[A](a: => A): IO[IORef[A]] = IO { new Ref[RealWorld, A](a) }
}
