package sio.core

import cats.{MonadError, Monad}

object instances {
  implicit val instance: Monad[IO] with MonadError[IO, Throwable] = new Monad[IO] with MonadError[IO, Throwable] {
    override def pure[A](x: A): IO[A] = IO.pure(x)
    override def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = fa.handleErrorWith(f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] = defaultTailRecM(a)(f)
  }
}
