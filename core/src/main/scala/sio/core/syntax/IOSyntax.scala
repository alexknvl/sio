package sio.core.syntax

import sio.core.{IO, LiftIO}
import sio.core.control.{RunInBase, MonadControlIO}

trait IOSyntax {
  implicit class IOSyntax[A] (val value: IO[A]) {
    /**
      * Lift this action into a given IO-like monad.
      */
    def liftIO[F[_]](implicit F: LiftIO[F]): F[A] = F.liftIO(value)

    def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit M: MonadControlIO[M]): M[B] =
      M.control((runInIO: RunInBase[M, IO]) => value.bracket(after)(during andThen runInIO.apply))
  }
}
