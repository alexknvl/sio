package sio.core

import simulacrum.typeclass

/**
  * MonadControlIO is the class of IO-based monads supporting an extra operation liftControlIO,
  * enabling control operations on IO to be lifted into the monad.
  */
@typeclass trait MonadControlIO[F[_]] extends LiftControlIO[F] with MonadIO[F] {
  /**
    * liftControlIO is a version of liftControl that operates through an arbitrary stack of
    * monad transformers directly to an inner IO (analagously to how liftIO is a version of lift).
    * So it can be used to lift control operations on IO into any monad in MonadControlIO.
    *
    * For example:
    * {{{
    *   def foo[A](a: IO[A]): IO[A]
    *   def fooControl[F[_], A](a: F[A])(implicit F: MonadControlIO[F]): F[A] =
    *     controlIO(runInIO => foo(runInIO(a)))
    * }}}
    *
    * Instances should satisfy similar laws as the MonadIO laws:
    *   liftControlIO . const . return = return
    *   liftControlIO (const (m >>= f)) = liftControlIO (const m) >>= liftControlIO . const . f
    * Additionally instances should satisfy:
    *   controlIO $ \runInIO -> runInIO m = m
    */

  def controlIO[A](f: RunInBase[F, IO] => IO[F[A]]): F[A] = flatten(liftControlIO(f))
}

object MonadControlIO {
  implicit val ioInstance: MonadControlIO[IO] = new MonadControlIO[IO] {
    override def pure[A](x: A): IO[A] = IO.pure(x)
    override def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)

    override def liftIO[A](a: IO[A]): IO[A] = a
    override def liftControlIO[A](f: (RunInBase[IO, IO]) => IO[A]): IO[A] = f(new RunInBase[IO, IO] {
      override def apply[B](x: IO[B]): IO[IO[B]] = x.map(IO.pure)
    })

    override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] = defaultTailRecM(a)(f)
    override def handleErrorWith[A](fa: IO[A])(f: (Throwable) => IO[A]): IO[A] = fa.handleErrorWith(f)
  }
}