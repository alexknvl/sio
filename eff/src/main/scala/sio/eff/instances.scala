package sio.eff

import cats.MonadError
import sio.core.IO
import ops.Union

object instances {
  implicit def instance[E <: EffectSet]: MonadError[EffIO[E, ?], Throwable] = new MonadError[EffIO[E, ?], Throwable] {
    override def pure[A](x: A): EffIO[E, A] =
      EffIO.pure(x)
    override def raiseError[A](e: Throwable): EffIO[E, A] =
      EffIO.lift(IO.raiseError(e))
    override def map[A, B](fa: EffIO[E, A])(f: A => B): EffIO[E, B] =
      fa.map(f)
    override def flatMap[A, B](fa: EffIO[E, A])(f: A => EffIO[E, B]): EffIO[E, B] =
      fa.flatMap[B, E, E](f)(Union.idempotence[E])
    override def handleErrorWith[A](fa: EffIO[E, A])(f: Throwable => EffIO[E, A]): EffIO[E, A] =
      fa.handleErrorWith[E, E](f)(Union.idempotence[E])
    override def tailRecM[A, B](a: A)(f: (A) => EffIO[E, Either[A, B]]): EffIO[E, B] =
      defaultTailRecM(a)(f)
  }
}
