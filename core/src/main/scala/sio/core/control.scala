package sio.core

import cats.data._
import cats.kernel.Monoid
import cats.{Applicative, TransLift, Monad}
import simulacrum.typeclass

object control {
  // type Run t = forall n o β. (Monad n, Monad o, Monad (t o)) => t n β -> n (t o β)
  trait Run[T[_[_], _]] {
    def apply[F[_], G[_], A](tfa: T[F, A])(implicit F: Monad[F], G: Monad[G], TG: Monad[T[G, ?]]): F[T[G, A]]
  }

  trait MonadTransControl[T[_[_], _]] extends TransLift[T] {
    type TC[M[_]] = cats.Monad[M]

    def liftControl[M[_], A](f: Run[T] => M[A])(implicit M: Monad[M]): T[M, A]
    def control[M[_], A](f: Run[T] => M[T[M, A]])(implicit M: Monad[M], TM: Monad[T[M, ?]]): T[M, A] =
      TM.flatten(liftControl(f)(M))
  }
  object MonadTransControl {
    /*
    liftControlNoState ∷ (Monad m, Monad f)
                   ⇒ (∀ p β. p (f β) → t p β)
                   → (∀ n β. t n β → n (f β))
                   → ((Run t → m α) → t m α)
    liftControlNoState mkT runT = \f → mkT $ liftM return $ f $
                                     liftM (mkT ∘ return) ∘ runT
     */

    def noState[T[_[_], _], TC[_[_]], F[_], G[_], X](mkT: F[X] => T[F, X], runT: T[F, X] => G[F[X]])(implicit T: TransLift.Aux[T, TC]): MonadTransControl[T] = new MonadTransControl[T] {
      override def liftControl[M[_], A](f: (Run[T]) => M[A])(implicit M: Monad[M]): T[M, A] = ???
      override def liftT[M[_], A](ma: M[A])(implicit evidence$1: Monad[M]): T[M, A] = ???
    }

    implicit def idTLiftIO[F[_]](implicit F: LiftIO[F]): LiftIO[IdT[F, ?]] = new LiftIO[IdT[F, ?]] {
      def liftIO[A](a: IO[A]) = IdT(F.liftIO(a))
    }

    implicit val optionTMonadTransControl: MonadTransControl[OptionT] = new MonadTransControl[OptionT] {
      override def liftT[M[_], A](ma: M[A])(implicit M: Monad[M]): OptionT[M, A] =
        OptionT(M.map(ma)(Option.apply))
      override def liftControl[M[_], A](f: Run[OptionT] => M[A])(implicit M: Monad[M]): OptionT[M, A] = ???
    }

    implicit def xorTLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[XorT[F, E, ?]] = new LiftIO[XorT[F, E, ?]] {
      def liftIO[A](a: IO[A]) = XorT(F.liftIO(a.map(Xor.right[E, A])))
    }

    implicit def eitherTLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[EitherT[F, E, ?]] = new LiftIO[EitherT[F, E, ?]] {
      def liftIO[A](a: IO[A]) = EitherT(F.liftIO(a.map(Right[E, A])))
    }

    implicit def kleisliLiftIO[F[_], E](implicit F: LiftIO[F]): LiftIO[Kleisli[F, E, ?]] = new LiftIO[Kleisli[F, E, ?]] {
      def liftIO[A](a: IO[A]) = Kleisli(_ => F.liftIO(a))
    }

    implicit def writerTLiftIO[F[_], W: Monoid](implicit F: LiftIO[F]): LiftIO[WriterT[F, W, ?]] = new LiftIO[WriterT[F, W, ?]] {
      def liftIO[A](a: IO[A]) = WriterT(F.liftIO(a.map((Monoid[W].empty, _))))
    }

    implicit def stateTLiftIO[F[_], S](implicit M: Monad[F], F: LiftIO[F]): LiftIO[StateT[F, S, ?]] = new LiftIO[StateT[F, S, ?]] {
      def liftIO[A](ioa: IO[A]) = StateT(s => F.liftIO(ioa.map((s, _))))
    }
  }

  // type RunInBase m base = forall β. m β -> base (m β)
  //
  // type Run t = forall n o β. (Monad n, Monad o, Monad (t o)) => t n β -> n (t o β)
  // given t n = m, n = base, Monad base, Monad m
  // Run t = forall β. m β -> base (m β) = RunInBase m base
  trait RunInBase[M[_], Base[_]] {
    def apply[A](x: M[A]): Base[M[A]]
  }

  trait MonadControl[F[_], B[_]] extends Monad[F] {
    def liftControl[A](f: RunInBase[F, B] => B[A]): F[A]
    def control[A](f: RunInBase[F, B] => B[F[A]]): F[A] = flatten(liftControl(f))
  }

  object MonadControl {
    def id[M[_]](implicit M: Monad[M]): MonadControl[M, M] = new MonadControl[M, M] {
      override def liftControl[A](f: (RunInBase[M, M]) => M[A]): M[A] =
        f(new RunInBase[M, M] { def apply[B](x: M[B]): M[M[B]] = M.map(x)(M.pure) })

      override def pure[A](x: A): M[A] = M.pure(x)
      override def flatMap[A, B](fa: M[A])(f: (A) => M[B]): M[B] = M.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: (A) => M[Either[A, B]]): M[B] = M.tailRecM(a)(f)
    }
  }

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

  object MonadControlIO {
    implicit val ioInstance: MonadControlIO[IO] = new MonadControlIO[IO] {
      override def pure[A](x: A): IO[A] = IO.pure(x)
      override def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)

      override def liftIO[A](a: IO[A]): IO[A] = a
      override def liftControl[A](f: (RunInBase[IO, IO]) => IO[A]): IO[A] = f(new RunInBase[IO, IO] {
        override def apply[B](x: IO[B]): IO[IO[B]] = x.map(IO.pure)
      })

      override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa.flatMap(f)
      override def tailRecM[A, B](a: A)(f: (A) => IO[Either[A, B]]): IO[B] = defaultTailRecM(a)(f)
      override def handleErrorWith[A](fa: IO[A])(f: (Throwable) => IO[A]): IO[A] = fa.handleErrorWith(f)
    }
  }
}
