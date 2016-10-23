package sio.core

import cats.Monad
import cats.data._
import cats.kernel.Monoid
import simulacrum.typeclass

@typeclass trait LiftIO[F[_]] extends Capture[F] {
  def capture[A](a: => A): F[A] = liftIO(IO(a))
  def liftIO[A](a: IO[A]): F[A]
}

object LiftIO {
  implicit def idTLiftIO[F[_]](implicit F: LiftIO[F]): LiftIO[IdT[F, ?]] = new LiftIO[IdT[F, ?]] {
    def liftIO[A](a: IO[A]) = IdT(F.liftIO(a))
  }

  implicit def optionTLiftIO[F[_]](implicit F: LiftIO[F]): LiftIO[OptionT[F, ?]] = new LiftIO[OptionT[F, ?]] {
    def liftIO[A](a: IO[A]) = OptionT(F.liftIO(a.map(Some(_): Option[A])))
  }

  implicit def xorTLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[XorT[F, E, ?]] = new LiftIO[XorT[F, E, ?]] {
    def liftIO[A](a: IO[A]) = XorT(F.liftIO(a.map(Xor.right[E, A])))
  }

  implicit def eitherTLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[EitherT[F, E, ?]] = new LiftIO[EitherT[F, E, ?]] {
    def liftIO[A](a: IO[A]) = EitherT(F.liftIO(a.map(Right[E, A])))
  }

  implicit def kleisliLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[Kleisli[F, E, ?]] = new LiftIO[Kleisli[F, E, ?]] {
    def liftIO[A](a: IO[A]) = Kleisli(_ => F.liftIO(a))
  }

  implicit def writerTLiftIO[F[_], W: Monoid](implicit F: LiftIO[F]): LiftIO[WriterT[F, W, ?]] = new LiftIO[WriterT[F, W, ?]] {
    def liftIO[A](a: IO[A]) = WriterT(F.liftIO(a.map((Monoid[W].empty, _))))
  }

  implicit def stateTLiftIO[F[_], S](implicit M: Monad[F], F: LiftIO[F]): LiftIO[StateT[F, S, ?]] = new LiftIO[StateT[F, S, ?]] {
    def liftIO[A](ioa: IO[A]) = StateT(s => F.liftIO(ioa.map((s, _))))
  }
}