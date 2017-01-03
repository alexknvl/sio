package sio.core

sealed abstract class IOOp[S, A] extends Product with Serializable
object IOOp {
  final case class Lift[S, A](run: () => Impure[A]) extends IOOp[S, A]
  final case class Unlift[A](run: IO[A]) extends IOOp[World.Real, () => A]
}
