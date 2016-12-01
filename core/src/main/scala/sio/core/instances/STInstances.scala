package sio.core.instances

import cats.{RecursiveTailRecM, MonadError}
import sio.base.free.FreeME
import sio.core.ST

trait STInstances {
  implicit def stMonadError[S]: MonadError[ST[S, ?], Throwable] with RecursiveTailRecM[ST[S, ?]] =
    new MonadError[ST[S, ?], Throwable] with RecursiveTailRecM[ST[S, ?]] {
      override def pure[A](x: A): ST[S, A] = ST.pure(x)
      override def raiseError[A](e: Throwable): ST[S, A] = ST.raiseError(e)

      override def map[A, B](fa: ST[S, A])(f: A => B): ST[S, B] =
        new ST(fa.value.map(f))
      override def flatMap[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
        new ST(fa.value.flatMap(a => f(a).value))
      override def tailRecM[A, B](a: A)(f: A => ST[S, Either[A, B]]): ST[S, B] =
        new ST(FreeME.instance.tailRecM(a)(a => f(a).value))
      override def handleErrorWith[A](fa: ST[S, A])(f: Throwable => ST[S, A]): ST[S, A] =
        new ST(fa.value.handleErrorWith(t => f(t).value))
    }
}
