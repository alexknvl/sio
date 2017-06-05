package sio.core

import cats.~>
import cats.syntax.either._
import cats.instances.either._

import leibniz.Forall
import sio.base.free.FreeRM

object `package` {
  type Impure[+A] = A

  type ST[S, +A] = ST.T[S, A]
  final val ST = new STImpl {
    type I[A] = Either[Throwable, A]
    type F[A] = () => A
    type T[S, +A] = FreeRM[I, F, A]

    def unit[S]: T[S, Unit] =
      FreeRM.pure[I, F, Unit](Right(()))
    def pure[S, A](x: A): T[S, A] =
      FreeRM.pure[I, F, A](Right(x))
    def raise[S](x: Throwable): T[S, Nothing] =
      FreeRM.pure[I, F, Nothing](Left(x))

    def map[S, A, B](fa: T[S, A], f: A => B): T[S, B] =
      FreeRM.map[I, F, A, B](fa)(f)
    def flatMap[S, A, B](fa: T[S, A], f: A => T[S, B]): T[S, B] =
      FreeRM.bind[I, F, A, B](fa) {
        case Left(x) => FreeRM.pure[I, F, B](Left(x))
        case Right(x) => f(x)
      }
    def handleErrorWith[S, A](fa: T[S, A], f: Throwable => T[S, A]): T[S, A] =
      FreeRM.bind[I, F, A, A](fa) {
        case Left(x) => f(x)
        case Right(x) => FreeRM.pure[I, F, A](Right(x))
      }

    def unsafeCapture[S, A](a: => Impure[A]): T[S, A] =
      FreeRM.suspend[I, F, A](() => a)

    def unsafeCallback[S, A, B](action: A => T[S, B]): T[S, A => Impure[B]] =
      pure[S, A => Impure[B]] { a =>
        FreeRM.foldMap[Either[Throwable, ?], F, B](action(a), stInterpreter)
          .fold(e => throw e, identity)
      }

    def unsafeCallback0[S, A](action: T[S, A]): T[S, () => Impure[A]] =
      pure[S, () => Impure[A]] { () =>
        FreeRM.foldEither[Throwable, F, A](action, stInterpreter)
          .fold(e => throw e, identity)
      }

    def trace[S](s: String): T[S, Unit] =
      unsafeCapture[S, Unit] { System.err.println(s) }

    private[this] val stInterpreter: F ~> Either[Throwable, ?] =
      new (F ~> Either[Throwable, ?]) {
        override def apply[B](f: F[B]): Either[Throwable, B] =
          Either.catchNonFatal(f())
      }

    def attempt[A](forallST: Forall[T[?, A]]): Either[Throwable, A] =
      attemptReal[A](forallST.apply[World.Real])

    def unsafeRun[A](forallST: Forall[T[?, A]]): A =
      unsafeRunReal[A](forallST.apply[World.Real])

    def attemptReal[A](action: T[World.Real, A]): Either[Throwable, A] =
      FreeRM.foldEither[Throwable, F, A](action, stInterpreter)

    def unsafeRunReal[A](action: T[World.Real, A]): A =
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
