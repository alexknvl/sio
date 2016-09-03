package sio.core

object instances {
  implicit val instance: MonadIO[IO] = new MonadIO[IO] {
    override def pure[A](x: A): IO[A] = IO.pure(x)
    override def capture[A](a: => A): IO[A] = IO(a)

    override def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = fa.handleErrorWith(f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] = defaultTailRecM(a)(f)
  }
}
