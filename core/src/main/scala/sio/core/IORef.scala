package sio.core


object IORef {
  def create[A](a: A): ST[RW, IORef[A]] = IO { new Ref[RW, A](a) }
}
