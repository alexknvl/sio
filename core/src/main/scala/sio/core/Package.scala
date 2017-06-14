package sio.core

import java.util.concurrent.Callable

import leibniz.{===, Forall}
import sio.base.free.`package`.RealIO

import cats.data.EitherT
import cats.syntax.either._
import cats.instances.either._

object `package` {
  type ST[S, +A] = ST.T[S, A]
  final val ST: STImpl = new STImpl {
    type T[S, +A] = RealIO[A]

    final def unit[S]: T[S, Unit] =
      RealIO.unit
    final def pure[S, A](x: A): T[S, A] =
      RealIO.pure[A](x)
    final def raise[S](x: Throwable): T[S, Nothing] =
      RealIO.raise(x)

    final def map[S, A, B](fa: T[S, A])(f: A => B): T[S, B] =
      RealIO.map[A, B](fa)(f)
    final def flatMap[S, A, B](fa: T[S, A])(f: A => T[S, B]): T[S, B] =
      RealIO.flatMap[A, B](fa)(f)
    final def handleErrorWith[S, A](fa: T[S, A])(f: Throwable => T[S, A]): T[S, A] =
      RealIO.handle[A](fa)(f)

    final def unsafeCapture[S, A](a: => Impure[A]): T[S, A] =
      RealIO.delay((_: Any) => a)

    final def unsafeCallback[S, A, B](action: A => T[S, B]): T[S, A => Impure[B]] =
      pure[S, A => Impure[B]] { a =>
        RealIO.run[B](action(a)).fold(e => throw e, identity)
      }

    final def unsafeCallback0[S, A](action: T[S, A]): T[S, () => Impure[A]] =
      pure[S, () => Impure[A]] { () =>
        RealIO.run[A](action).fold(e => throw e, identity)
      }

    final def trace[S](s: String): T[S, Unit] =
      unsafeCapture[S, Unit] { System.err.println(s) }

    final def attempt[A](forallST: Forall[T[?, A]]): Either[Throwable, A] =
      attemptReal[A](forallST.apply[RW])

    final def unsafeRun[A](forallST: Forall[T[?, A]]): A =
      unsafeRunReal[A](forallST.apply[RW])

    final def attemptReal[A](action: T[RW, A]): Either[Throwable, A] =
      RealIO.run[A](action)

    final def unsafeRunReal[A](action: T[RW, A]): A =
      attemptReal[A](action).fold(e => throw e, identity)
  }

  type Mut[S, A] = Mut.T[S, A]
  final val Mut: MutImpl = new MutImpl {
    type T[S, A] = A
    def wrap[S, A](a: A): T[S, A] = a
    def unwrap[S, A](tsa: T[S, A]): A = tsa
    def subst[F[_], S, A](fa: F[A]): F[T[S, A]] = fa
  }

  type Impure[A]      = A

  type IO[A]           = ST[RW, A]

  type STRef[S, A]     = Ref[S, A]
  type STMutable[S, A] = Mut[S, A]
  type STArray[S, E]   = Mut[S, Array[E]]

  type IORef[A]        = Ref[RW, A]
  type IOMutable[A]    = Mut[RW, A]
  type IOArray[E]      = Mut[RW, Array[E]]

  type ForallST[A]     = Forall[ST[?, A]]
}
