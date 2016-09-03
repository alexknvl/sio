package sio.core

import simulacrum.typeclass

@typeclass trait Capture[F[_]] {
  def capture[A](f: => A): F[A]
}