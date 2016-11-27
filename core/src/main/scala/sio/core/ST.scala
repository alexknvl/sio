package sio.core

import cats.{MonadError, Monad}

import scala.language.implicitConversions

final case class ST[S, A](unsafeUnwrap: dmz.RealIO[A]) {
  def io: IO[A] = new IO(unsafeUnwrap)
}

object ST {
  def attempt[A](f: Forall[ST[?, A]]): Either[Throwable, A] = f.apply.unsafeUnwrap.attempt()
  def unsafeRun[A](f: Forall[ST[?, A]]): A = f.apply.unsafeUnwrap.run()

  def unit[S]: ST[S, Unit] = ST(dmz.unit)
  def pure[S, A](x: A): ST[S, A] = ST(dmz.pure(x))
  def raiseError[S, A](e: Throwable): ST[S, A] = ST(dmz.raiseError(e))
  def trace[S](s: String): ST[S, Unit] = ST(dmz.capture { System.err.println(s) })

  implicit def instance[S]: Monad[ST[S, ?]] with MonadError[ST[S, ?], Throwable] = new Monad[ST[S, ?]] with MonadError[ST[S, ?], Throwable] {
    override def pure[A](x: A): ST[S, A] =
      ST.pure(x)
    override def map[A, B](fa: ST[S, A])(f: A => B): ST[S, B] =
      ST(fa.unsafeUnwrap.map(f))
    override def flatMap[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
      ST(fa.unsafeUnwrap.flatMap(a => f(a).unsafeUnwrap))
    override def tailRecM[A, B](a: A)(f: A => ST[S, Either[A, B]]): ST[S, B] =
      ST(dmz.tailRecM(a)(a => f(a).unsafeUnwrap))

    override def raiseError[A](e: Throwable): ST[S, A] =
      ST.raiseError(e)
    override def handleErrorWith[A](fa: ST[S, A])(f: Throwable => ST[S, A]): ST[S, A] =
      ST(fa.unsafeUnwrap.handleErrorWith(t => f(t).unsafeUnwrap))
  }
}