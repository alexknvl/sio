package sio.base

// type RunInBase m base = forall β. m β -> base (m β)
trait RunInBase[M[_], Base[_]] {
  def apply[A](x: M[A]): Base[M[A]]
}