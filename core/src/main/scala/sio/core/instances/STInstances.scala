package sio.core.instances

import cats.MonadError
import sio.core.ST
import sio.dmz.RealIO

trait STInstances {
  implicit def stMonadError[S]: MonadError[ST[S, ?], Throwable] = new MonadError[ST[S, ?], Throwable] {
    override def pure[A](x: A): ST[S, A] =
      ST.pure(x)
    override def map[A, B](fa: ST[S, A])(f: A => B): ST[S, B] =
      ST(fa.unsafeUnwrap.map(f))
    override def flatMap[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
      ST(fa.unsafeUnwrap.flatMap(a => f(a).unsafeUnwrap))
    override def tailRecM[A, B](a: A)(f: A => ST[S, Either[A, B]]): ST[S, B] =
      ST(RealIO.tailRecM(a)(a => f(a).unsafeUnwrap))

    override def raiseError[A](e: Throwable): ST[S, A] =
      ST.raiseError(e)
    override def handleErrorWith[A](fa: ST[S, A])(f: Throwable => ST[S, A]): ST[S, A] =
      ST(fa.unsafeUnwrap.handleErrorWith(t => f(t).unsafeUnwrap))
  }
}
