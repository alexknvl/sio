package sio.core

import simulacrum.typeclass


@typeclass trait LiftIO[F[_]] {
  def liftIO[A](a: IO[A]): F[A]
  def capture[A](a: => Impure[A]): F[A] = liftIO(IO(a))
}