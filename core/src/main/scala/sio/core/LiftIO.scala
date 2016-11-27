package sio.core

import cats.Monad
import cats.data._
import cats.syntax.functor._
import cats.kernel.Monoid
import simulacrum.typeclass

@typeclass trait LiftIO[F[_]] {
  def liftIO[A](a: IO[A]): F[A]
  def capture[A](a: => A): F[A] = liftIO(IO(a))
}