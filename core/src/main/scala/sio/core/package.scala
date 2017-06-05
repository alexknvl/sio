package sio.core

import cats.~>
import cats.syntax.either._
import cats.instances.either._

import leibniz.Forall
import sio.base.free._

object `package` {
  type Impure[+A] = A

  type ST[S, +A] = ST.T[S, A]
  final val ST = new STImpl {
    type I[A] = Either[Throwable, A]
    type F[A] = () => A
    type T[S, +A] = RealIO[A]

    final def unit[S]: T[S, Unit] =
      RealIO.unit
    final def pure[S, A](x: A): T[S, A] =
      RealIO.pure[A](x)
    final def raise[S](x: Throwable): T[S, Nothing] =
      RealIO.raise(x)

    final def map[S, A, B](fa: T[S, A], f: A => B): T[S, B] =
      RealIO.map[A, B](fa)(f)
    final def flatMap[S, A, B](fa: T[S, A], f: A => T[S, B]): T[S, B] =
      RealIO.flatMap[A, B](fa)(f)
    final def handleErrorWith[S, A](fa: T[S, A], f: Throwable => T[S, A]): T[S, A] =
      RealIO.handle[A](fa)(f)

    final def unsafeCapture[S, A](a: => Impure[A]): T[S, A] =
      RealIO.map(RealIO.unit)(_ => a)

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
      attemptReal[A](forallST.apply[World.Real])

    final def unsafeRun[A](forallST: Forall[T[?, A]]): A =
      unsafeRunReal[A](forallST.apply[World.Real])

    final def attemptReal[A](action: T[World.Real, A]): Either[Throwable, A] =
      RealIO.run[A](action)

    final def unsafeRunReal[A](action: T[World.Real, A]): A =
      attemptReal[A](action).fold(e => throw e, identity)
  }

  type Mutable[S, A] = Mutable.T[S, A]
  final val Mutable: MutableImpl = new MutableImpl {
    type T[S, A] = A
    def wrap[S, A](a: A): T[S, A] = a
    def unwrap[S, A](tsa: T[S, A]): A = tsa
    def subst[F[_], S, A](fa: F[A]): F[T[S, A]] = fa
  }

  type IO[A] = ST[World.Real, A]

  type STRef[S, A] = Ref[S, A]
  type STMutable[S, A] = Mutable[S, A]
  type STArray[S, E] = Mutable[S, Array[E]]

  type IORef[A] = Ref[World.Real, A]
  type IOMutable[A] = Mutable[World.Real, A]
  type IOArray[E] = IOMutable[Array[E]]

  type ForallST[A] = Forall[ST[?, A]]
}
