package sio.core.instances

import cats.MonadError
import sio.core.ST

trait STInstances {
  implicit def stMonadError[S]: MonadError[ST[S, ?], Throwable] =
    new MonadError[ST[S, ?], Throwable] {
      override def pure[A](x: A): ST[S, A] = ST.pure(x)
      override def raiseError[A](e: Throwable): ST[S, A] = ST.raiseError(e)

      override def map[A, B](fa: ST[S, A])(f: A => B): ST[S, B] =
        new ST(fa.value.map(f))
      override def flatMap[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
        new ST(fa.value.flatMap(a => f(a).value))
      override def handleErrorWith[A](fa: ST[S, A])(f: Throwable => ST[S, A]): ST[S, A] =
        new ST(fa.value.handleErrorWith(t => f(t).value))

      override def tailRecM[A, B](a: A)(f: A => ST[S, Either[A, B]]): ST[S, B] =
        pure(a).flatMap(a => f(a).flatMap {
          case Right(b) => pure(b)
          case Left(x) => tailRecM(x)(f)
        })
    }
}
