package sio.core.syntax

import sio.base.RunInBase
import sio.core.{MonadControlIO, IO, LiftIO}

import st._

final class IOSyntaxOps[A] (val value: IO[A]) extends AnyVal {
  /**
    * Lift this action into a given IO-like monad.
    */
  def liftIO[F[_]](implicit F: LiftIO[F]): F[A] = F.liftIO(value)

  def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit M: MonadControlIO[M]): M[B] =
    M.control((runInIO: RunInBase[M, IO]) => value.bracket(after)(during andThen runInIO.apply))
}

trait IOSyntax extends STSyntax {
  implicit def toIOyntaxOps[A](st: IO[A]): IOSyntaxOps[A] =
    new IOSyntaxOps[A](st)
}
