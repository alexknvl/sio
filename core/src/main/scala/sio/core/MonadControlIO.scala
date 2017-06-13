package sio.core

import simulacrum.typeclass
import sio.base.MonadControl

/**
  * MonadControlIO is the class of IO-based monads supporting an extra operation liftControlIO,
  * enabling control operations on IO to be lifted into the monad.
  *
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
@typeclass trait MonadControlIO[F[_]] extends MonadIO[F] with MonadControl[F, IO]