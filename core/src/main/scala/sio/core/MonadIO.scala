package sio.core

import cats.{MonadError, Monad}
import simulacrum.typeclass

@typeclass trait MonadIO[F[_]] extends Monad[F] with MonadError[F, Throwable] with LiftIO[F]