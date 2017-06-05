package sio.core.instances

import cats.MonadError
import sio.base.RunInBase
import sio.core._

trait IOInstances extends STInstances { self: STInstances =>
  implicit val ioMonadIO: MonadIO[IO] with MonadControlIO[IO] = new MonadIO[IO] with MonadControlIO[IO] {
    private val M: MonadError[ST[World.Real, ?], Throwable] = self.stMonadError

    override def pure[A](x: A): IO[A] =
      M.pure(x)
    override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
      ST.flatMap(fa, f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] =
      M.tailRecM(a)(f)

    override def raiseError[A](e: Throwable): IO[A] =
      ST.raise(e)
    override def handleErrorWith[A](fa: IO[A])(f: (Throwable) => IO[A]): IO[A] =
      ST.handleErrorWith(fa, f)

    override def liftIO[A](a: IO[A]): IO[A] = a
    override def liftControl[A](f: (RunInBase[IO, IO]) => IO[A]): IO[A] = f(new RunInBase[IO, IO] {
      override def apply[B](x: IO[B]): IO[IO[B]] = ST.map(x, IO.pure)
    })
  }
}
