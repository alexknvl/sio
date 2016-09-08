package sio.regions

import simulacrum.typeclass
import sio.core.IO

@typeclass trait LiftControlIO[F[_]] {
  def liftControlIO[A](f: RunInBase[F, IO] => IO[A]): F[A]
}
