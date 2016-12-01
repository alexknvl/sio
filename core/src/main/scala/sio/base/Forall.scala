package sio.base

trait Forall[F[_]] {
  def apply[A]: F[A]
}