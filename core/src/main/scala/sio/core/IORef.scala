package sio.core

object IORef {
  def create[A](a: A): IO[IORef[A]] = IO { new Ref[World.Real, A](a) }
}
