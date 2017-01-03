package sio.core

sealed abstract class Op[S, A] extends Product with Serializable
object Op {
  final case class Lift[S, A](run: () => Impure[A]) extends Op[S, A]
  final case class Unlift[A](run: IO[A]) extends Op[World.Real, () => A]
}
