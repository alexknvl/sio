package sio

import cats.Id
import leibniz.Forall

package object core {
  type Impure[A] = Id[A]

  type Mutable[S, A] = MutableImpl.T[S, A]

  trait MutableImpl {
    type T[S, A]
    def wrap[S, A](a: A): T[S, A]
    def unwrap[S, A](tsa: T[S, A]): A
    def subst[F[_], S, A](fa: F[A]): F[T[S, A]]
  }
  val MutableImpl: MutableImpl = new MutableImpl {
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
