package sio.base

import cats.Functor

trait RelMonad[I[_], F[_]] extends Functor[F] {
  def pure[A](a: I[A]): F[A]
  def bind[A, B](fa: F[A])(f: I[A] => F[B]): F[B]
}
