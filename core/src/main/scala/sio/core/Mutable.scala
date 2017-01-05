package sio.core

final class Mutable[S, A](val unsafeValue: A) extends AnyVal {
  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"Mutable[${unsafeValue.getClass.getSimpleName}]@$id"
}

object Mutable {
  abstract class Syntax[S, A](val handle: Mutable[S, A]) {
    @inline final def lift[B](f: A => Impure[B]): ST[S, B] = ST.unsafeCapture { f(handle.unsafeValue) }
    @inline final def unit[B](f: A => Impure[B]): ST[S, Unit] = ST.unsafeCapture { f(handle.unsafeValue); () }
    @inline final def unsafePure[B](f: A => B): B = f(handle.unsafeValue)
  }
}