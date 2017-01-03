package sio.base

import cats.Monad

trait MonadControl[F[_], B[_]] extends Monad[F] {
  def liftControl[A](f: RunInBase[F, B] => B[A]): F[A]
  def control[A](f: RunInBase[F, B] => B[F[A]]): F[A] = flatten(liftControl(f))
}

object MonadControl {
  def id[M[_]](implicit M: Monad[M]): MonadControl[M, M] = new MonadControl[M, M] {
    override def liftControl[A](f: (RunInBase[M, M]) => M[A]): M[A] =
      f(new RunInBase[M, M] { def apply[B](x: M[B]): M[M[B]] = M.map(x)(M.pure) })

    override def pure[A](x: A): M[A] = M.pure(x)
    override def flatMap[A, B](fa: M[A])(f: (A) => M[B]): M[B] = M.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: (A) => M[Either[A, B]]): M[B] = M.tailRecM(a)(f)
  }
}
