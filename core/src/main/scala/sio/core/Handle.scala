package sio.core

final class Handle[S, A](val unsafeValue: A) {
  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"Handle[${unsafeValue.getClass.getSimpleName}]@$id"
}

object Handle {
  abstract class Syntax[S, A](val handle: Handle[S, A]) {
    def lift[B](f: A => B): ST[S, B] = ST.unsafeCapture { f(handle.unsafeValue) }
    def unit[B](f: A => B): ST[S, Unit] = ST.unsafeCapture { f(handle.unsafeValue); () }
  }
}