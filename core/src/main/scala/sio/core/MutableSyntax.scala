package sio.core

trait MutableImpl {
  type T[S, A]
  def wrap[S, A](a: A): T[S, A]
  def unwrap[S, A](tsa: T[S, A]): A
  def subst[F[_], S, A](fa: F[A]): F[T[S, A]]
}

abstract class MutableSyntax[S, A](val handle: Mutable[S, A]) {
  @inline final def lift[B](f: A => Impure[B]): ST[S, B] = ST.unsafeCapture { f(Mutable.unwrap(handle)) }
  @inline final def unit[B](f: A => Impure[B]): ST[S, Unit] = ST.unsafeCapture { f(Mutable.unwrap(handle)); () }
  @inline final def unsafePure[B](f: A => B): B = f(Mutable.unwrap(handle))
}