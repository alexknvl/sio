package sio.core

trait Forall[F[_]] {
  def apply[A]: F[A]
}