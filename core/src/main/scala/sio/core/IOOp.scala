package sio.core

sealed abstract class IOOp[S, A] extends Product with Serializable
object IOOp {
  final case class Lift[S, A](run: () => Impure[A]) extends IOOp[S, A]
  final case class Unlift[S, A, B](run: A => ST[S, B]) extends IOOp[S, A => Impure[B]]
}
