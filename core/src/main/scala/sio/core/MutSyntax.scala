package sio.core

trait MutImpl {
  type T[S, A]
  def wrap[S, A](a: A): T[S, A]
  def unwrap[S, A](tsa: T[S, A]): A
  def subst[F[_], S, A](fa: F[A]): F[T[S, A]]
}

abstract class MutSyntax[S, A](val handle: Mut[S, A]) {
  @inline
  protected[this] final def lift[B](f: A => Impure[B]): ST[S, B] =
    ST.unsafeCapture { f(Mut.unwrap(handle)) }

  @inline
  protected[this] final def unit[B](f: A => Impure[B]): ST[S, Unit] =
    ST.unsafeCapture { f(Mut.unwrap(handle)); () }

  @inline
  protected[this] final def unsafePure[B](f: A => B): B =
    f(Mut.unwrap(handle))
}