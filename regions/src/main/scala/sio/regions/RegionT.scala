package sio.regions

import cats.Monad
import cats.data.{ReaderT, Kleisli}
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.list._
import sio.core.{MonadIO, IO}
import sio.ioref.IORef
import sio.ioref.newIORef

/**
  * A monad transformer in which scarce resources can be opened.
  * When the region terminates, all opened resources will be closed
  * automatically. It's a type error to return an opened resource
  * from the region, and no I/O with closed resources is possible.
  *
  * @tparam S a phantom type that prevents us from leaking (?)
  * @tparam P the base monad.
  * @tparam A the result type.
  */
final case class RegionT[S, P[_], A](run: ReaderT[P, IORef[List[RefCountedFinalizer]], A]) extends AnyVal

object RegionT {
  def readerTMonad[M[_]](implicit M: Monad[M]) = Monad[ReaderT[M, IORef[List[RefCountedFinalizer]], ?]]

  def liftIO[S, M[_], A](a: IO[A])(implicit M: MonadIO[M]): RegionT[S, M, A] = RegionT(ReaderT(_ => M.liftIO(a)))

  implicit def regionTMonad[S, M[_]](implicit M: MonadIO[M]): MonadIO[RegionT[S, M, ?]] = new MonadIO[RegionT[S, M, ?]] {
    val RM = readerTMonad[M]

    override def pure[A](x: A): RegionT[S, M, A] =
      RegionT(ReaderT(_ => M.pure(x)))
    override def liftIO[A](a: IO[A]): RegionT[S, M, A] =
      RegionT(ReaderT(_ => M.liftIO(a)))
    override def raiseError[A](e: Throwable): RegionT[S, M, A] =
      RegionT(ReaderT(_ => M.raiseError(e)))

    override def flatMap[A, B](fa: RegionT[S, M, A])(f: A => RegionT[S, M, B]): RegionT[S, M, B] =
      RegionT(RM.flatMap(fa.run)(a => f(a).run))
    override def tailRecM[A, B](a: A)(f: A => RegionT[S, M, Either[A, B]]): RegionT[S, M, B] =
      RegionT(RM.tailRecM[A, B](a)(a => f(a).run))
    override def handleErrorWith[A](fa: RegionT[S, M, A])(f: Throwable => RegionT[S, M, A]): RegionT[S, M, A] =
      RegionT(ReaderT(s => M.handleErrorWith(fa.run.apply(s))(e => f(e).run.apply(s))))
  }
}

object `package` {
  trait Forall[F[_]] {
    def apply[A]: F[A]
  }

  /**
    * Forall[λ[X => (M[X] => Base[M[X]])]]
    */
  trait RunInBase[M[_], Base[_]] {
    def apply[A](x: M[A]): Base[M[A]]
  }

  implicit final class IOExtraOps[A](val io: IO[A]) extends AnyVal {
    def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit M: MonadControlIO[M]): M[B] =
      M.controlIO((runInIO: RunInBase[M, IO]) => io.bracket(after)(during andThen runInIO.apply))
  }

  def idLiftControl[M[_], A](f: RunInBase[M, M] => M[A])(implicit M: Monad[M]): M[A] =
    f(new RunInBase[M, M] { def apply[B](x: M[B]): M[M[B]] = x.map(M.pure) })

  @SuppressWarnings(Array("org.wartremover.warts.NoNeedForMonad"))
  private[this] def addFinalizer[F[_]](finalizersRef: IORef[List[RefCountedFinalizer]], finalizer: IO[Unit]): IO[FinalizerHandle[F]] =
    for {
      countRef <- newIORef[Int](1)
      h = RefCountedFinalizer(finalizer, countRef)
      _ <- finalizersRef.modify(h :: _)
    } yield FinalizerHandle[F](h)

  private[this] def exitBlock(finalizersRef: IORef[List[RefCountedFinalizer]]): IO[Unit] =
    finalizersRef.read.flatMap { finalizers =>
        finalizers.traverse_ { finalizer =>
          finalizer.refCount.modify(_ - 1)
            .flatMap(count => if (count == 0) finalizer.run else IO.unit)
        }
    }

  /**
    * Register a finalizer in the current region. When the region terminates,
    * all registered finalizers will be performed if they're not duplicated to a parent region.
    */
  def onExit[S, P[_] : MonadIO](finalizer: IO[Unit]): RegionT[S, P, FinalizerHandle[RegionT[S, P, ?]]] =
    RegionT(Kleisli[P, IORef[List[RefCountedFinalizer]], FinalizerHandle[RegionT[S, P, ?]]]{ finalizersRef =>
      addFinalizer[RegionT[S, P, ?]](finalizersRef, finalizer).liftIO[P]
    })

  /**
    * Execute a region inside its parent region P. All resources which have been opened in the given
    * region and which haven't been duplicated using "dup", will be closed on exit from this function
    * whether by normal termination or by raising an exception.
    * Also all resources which have been duplicated to this region from a child region are closed
    * on exit if they haven't been duplicated themselves.
    * The Forall quantifier prevents resources from being returned by this function.
    */
  def runRegionT[P[_], A](r: Forall[RegionT[?, P, A]])(implicit P: MonadControlIO[P]): P[A] =
    newIORef(List.empty[RefCountedFinalizer]).bracketIO(exitBlock)(r.apply.run.apply)
}