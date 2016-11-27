package sio.core

trait Forall[F[_]] {
  def apply[A]: F[A]
}

/**
  * Forall[λ[X => (M[X] => Base[M[X]])]]
  */
trait RunInBase[M[_], Base[_]] {
  def apply[A](x: M[A]): Base[M[A]]
}