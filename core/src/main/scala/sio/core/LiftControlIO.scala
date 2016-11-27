package sio.core

import simulacrum.typeclass

@typeclass trait LiftControlIO[F[_]] {
  def liftControlIO[A](f: RunInBase[F, IO] => IO[A]): F[A]
}
