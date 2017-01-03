package sio.eff

import cats.MonadError
import sio.core.IO
import ops.Union

object instances {
  implicit def instance[E <: EffectSet]: MonadError[EffIO[E, ?], Throwable] = new MonadError[EffIO[E, ?], Throwable] {
    private[this] val idemp = Union.idempotence[E]

    override def pure[A](x: A): EffIO[E, A] =
      EffIO.pure(x)
    override def raiseError[A](e: Throwable): EffIO[E, A] =
      EffIO.lift(IO.raiseError(e))
    override def map[A, B](fa: EffIO[E, A])(f: A => B): EffIO[E, B] =
      fa.map(f)
    override def flatMap[A, B](fa: EffIO[E, A])(f: A => EffIO[E, B]): EffIO[E, B] =
      fa.flatMap[B, E, E](f)(idemp)
    override def handleErrorWith[A](fa: EffIO[E, A])(f: Throwable => EffIO[E, A]): EffIO[E, A] =
      fa.handleErrorWith[E, E](f)(idemp)
    override def tailRecM[A, B](a: A)(f: (A) => EffIO[E, Either[A, B]]): EffIO[E, B] =
      pure(a).flatMap(a => f(a).flatMap {
        case Right(b) => pure(b)
        case Left(x) => tailRecM(x)(f)
      }(idemp))(idemp)
  }
}
