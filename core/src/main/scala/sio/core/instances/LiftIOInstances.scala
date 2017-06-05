package sio.core.instances

import cats.Monad
import cats.data._
import cats.kernel.Monoid
import sio.core._

trait LiftIOInstances {
  implicit def idTLiftIO[F[_]](implicit F: LiftIO[F]): LiftIO[IdT[F, ?]] = new LiftIO[IdT[F, ?]] {
    def liftIO[A](ioa: IO[A]) = IdT(F.liftIO[A](ioa))
  }

  implicit def optionTLiftIO[F[_]](implicit F: LiftIO[F]): LiftIO[OptionT[F, ?]] = new LiftIO[OptionT[F, ?]] {
    def liftIO[A](ioa: IO[A]) = OptionT(F.liftIO(ST.map(ioa, (a: A) => Some(a): Option[A])))
  }

  implicit def eitherTLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[EitherT[F, E, ?]] = new LiftIO[EitherT[F, E, ?]] {
    def liftIO[A](ioa: IO[A]) = EitherT(F.liftIO(ST.map(ioa, (a: A) => Right[E, A](a))))
  }

  implicit def kleisliLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[Kleisli[F, E, ?]] = new LiftIO[Kleisli[F, E, ?]] {
    def liftIO[A](ioa: IO[A]) = Kleisli(_ => F.liftIO(ioa))
  }

  implicit def writerTLiftIO[F[_], W: Monoid](implicit F: LiftIO[F]): LiftIO[WriterT[F, W, ?]] = new LiftIO[WriterT[F, W, ?]] {
    def liftIO[A](ioa: IO[A]) = WriterT(F.liftIO(ST.map(ioa, (a: A) => (Monoid[W].empty, a))))
  }

  implicit def stateTLiftIO[F[_], S](implicit M: Monad[F], F: LiftIO[F]): LiftIO[StateT[F, S, ?]] = new LiftIO[StateT[F, S, ?]] {
    def liftIO[A](ioa: IO[A]) = StateT(s => F.liftIO(ST.map(ioa, (a: A) => (s, a))))
  }
}
