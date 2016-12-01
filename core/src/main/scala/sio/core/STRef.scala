package sio.core

object STRef {
  def create[S, A](a: A): ST[S, Ref[S, A]] = ST.unsafeCapture { new Ref[S, A](a) }
}
