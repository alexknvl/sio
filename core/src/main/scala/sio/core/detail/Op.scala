package sio.core.detail

sealed abstract class Op[F[_], E, A, B] extends Product with Serializable
object Op {
  final case class Pure[F[_], E, A, B](f: Either[E, B]) extends Op[F, E, A, B]
  final case class Suspend[F[_], E, A, B](f: F[B]) extends Op[F, E, A, B]

  final case class Map[F[_], E, A, B](f: A => B) extends Op[F, E, A, B]
  final case class Bind[F[_], E, A, B](f: A => Thunk[F, E, Unit, B]) extends Op[F, E, A, B]
  final case class Handle[F[_], E, A](f: E => Thunk[F, E, Unit, A]) extends Op[F, E, A, A]
}
