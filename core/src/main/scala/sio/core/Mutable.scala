package sio.core

final class Mutable[S, A](val unsafeValue: A) {
  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"Mutable[${unsafeValue.getClass.getSimpleName}]@$id"
}

object Mutable {
  abstract class Syntax[S, A](val handle: Mutable[S, A]) {
    final def lift[B](f: A => B): ST[S, B] = ST.unsafeCapture { f(handle.unsafeValue) }
    final def unit[B](f: A => B): ST[S, Unit] = ST.unsafeCapture { f(handle.unsafeValue); () }
  }

  abstract class IOSyntax[A](val handle: IOMutable[A]) {
    final def lift[B](f: A => B): IO[B] = IO { f(handle.unsafeValue) }
    final def unit[B](f: A => B): IO[Unit] = IO { f(handle.unsafeValue); () }
  }
}