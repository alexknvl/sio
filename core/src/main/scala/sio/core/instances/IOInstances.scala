package sio.core.instances

import cats.MonadError
import sio.base.RunInBase
import sio.core._

trait IOInstances extends STInstances { self: STInstances =>
  implicit val ioMonadIO: MonadIO[IO] with MonadControlIO[IO] = new MonadIO[IO] with MonadControlIO[IO] {
    private val M: MonadError[ST[RW, ?], Throwable] = self.stMonadError

    override def pure[A](x: A): ST[RW, A] =
      M.pure(x)
    override def flatMap[A, B](fa: ST[RW, A])(f: (A) => ST[RW, B]): ST[RW, B] =
      ST.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: (A) => ST[RW, Either[A, B]]): ST[RW, B] =
      M.tailRecM(a)(f)

    override def raiseError[A](e: Throwable): ST[RW, A] =
      ST.raise(e)
    override def handleErrorWith[A](fa: ST[RW, A])(f: (Throwable) => ST[RW, A]): ST[RW, A] =
      ST.handleErrorWith(fa)(f)

    override def liftIO[A](a: ST[RW, A]): ST[RW, A] = a
    override def liftControl[A](f: (RunInBase[IO, IO]) => IO[A]): IO[A] = f(new RunInBase[IO, IO] {
      override def apply[B](x: ST[RW, B]): ST[RW, ST[RW, B]] = ST.map(x)(IO.pure)
    })
  }
}
