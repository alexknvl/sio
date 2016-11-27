package sio.core

import cats.MonadError

trait IOInstances { self: STInstances =>
  implicit val ioMonadIO: MonadIO[IO] = new MonadIO[IO] {
    private val M: MonadError[ST[RealWorld, ?], Throwable] = self.stMonadError

    override def pure[A](x: A): IO[A] =
      M.pure(x)
    override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
      M.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] =
      M.tailRecM(a)(f)

    override def raiseError[A](e: Throwable): IO[A] =
      M.raiseError(e)
    override def handleErrorWith[A](fa: IO[A])(f: (Throwable) => IO[A]): IO[A] =
      M.handleErrorWith(fa)(f)

    override def liftIO[A](a: IO[A]): IO[A] = a
  }
}
