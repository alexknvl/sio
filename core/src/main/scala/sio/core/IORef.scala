package sio.core

sealed abstract class IORef[A] {
  def read: IO[A]
  def write(a: => A): IO[Unit]
  def modify(f: A => A): IO[A]

  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"IORef[$id]"
}

object IORef {
  def create[A](a: A): IO[IORef[A]] = {
    def ref: IORef[A] = new IORef[A] {
      var unsafeValue = a
      def read: IO[A] = IO { unsafeValue }
      def write(a: => A): IO[Unit] = IO { unsafeValue = a }
      def modify(f: A => A): IO[A] = IO { unsafeValue = f(unsafeValue); unsafeValue }
    }

    IO { ref }
  }
}