package sio.core

abstract class MutableSyntax[S, A](val handle: Mutable[S, A]) {
  @inline final def lift[B](f: A => Impure[B]): ST[S, B] = ST.unsafeCapture { f(Mutable.unwrap(handle)) }
  @inline final def unit[B](f: A => Impure[B]): ST[S, Unit] = ST.unsafeCapture { f(Mutable.unwrap(handle)); () }
  @inline final def unsafePure[B](f: A => B): B = f(Mutable.unwrap(handle))
}