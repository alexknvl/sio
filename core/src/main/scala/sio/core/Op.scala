package sio.core

sealed abstract class Op[S, A] extends Product with Serializable
object Op {
  final case class Effect[S, A](run: () => A) extends Op[S, A]
  final case class ForkOS[A](run: IO[Unit]) extends Op[World.Real, IOMutable[Thread]]
}
