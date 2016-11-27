package sio

import java.util
import java.util.Comparator
import java.util.concurrent.Callable

import cats.kernel.Order
import sio.core.control.{RunInBase, MonadControlIO}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Sorting, Try}
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

  type STArray[S, E] = Handle[S, Array[E]]
  implicit class STArraySyntax[S, E](handle: STArray[S, E]) extends Handle.Syntax(handle) {
    def length: ST[S, Int] = lift { _.length }

    def get(i: Int): ST[S, E] = lift { _.apply(i) }
    def set(i: Int, x: E): ST[S, Unit] = lift { _.update(i, x) }

    def transform(f: E => E): ST[S, Unit] = lift { a =>
      @tailrec def go(i: Int): Unit =
        if (i < a.length) {
          a.update(i, f(a(i)))
          go(i + 1)
        } else ()
      go(0)
    }

    def stableSort(implicit E: Order[E], CT: ClassTag[E]): ST[S, Unit] =
      lift { a => Sorting.stableSort(a)(CT, E.toOrdering) }
    def quickSort(implicit E: Order[E], CT: ClassTag[E]): ST[S, Unit] =
      lift { a => Sorting.quickSort(a)(E.toOrdering) }
  }

  object STArray {
    def fill[S, A](len: Int)(value: => A)(implicit A: ClassTag[A]): ST[S, STArray[S, A]] =
      ST.unsafeCapture { new STArray[S, A](Array.fill(len)(value)) }
  }
}
