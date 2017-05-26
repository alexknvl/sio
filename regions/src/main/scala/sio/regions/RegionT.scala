package sio.regions

import cats.{ApplicativeError, MonadError, Monad}
import cats.data.{ReaderT, Kleisli}
import cats.syntax.all._
import cats.instances.list._
import leibniz.Forall
import sio.base.RunInBase
import sio.core._
import sio.core.instances.all._
import sio.core.syntax.io._

import scala.annotation.unchecked.{ uncheckedVariance => uV }

/**
  * A monad transformer in which scarce resources can be opened.
  * When the region terminates, all opened resources will be closed
  * automatically. It's a type error to return an opened resource
  * from the region, and no I/O with closed resources is possible.
  *
  * @tparam S a phantom type that prevents leaking.
  * @tparam F the base monad.
  * @tparam A the result type.
  */
final class RegionT[S, +F[_], +A](val run: ReaderT[F @uV, IORef[List[RefCountedFinalizer]], A @uV]) extends AnyVal

import RegionT._

trait RegionT3 {
  implicit def regionTMonadError[S, F[_]](implicit F: MonadError[F, Throwable]): MonadError[RegionT[S, F, ?], Throwable] =
    new RegionTMonadError[S, F](F)
}
trait RegionT2 extends RegionT3 {
  implicit def regionTMonadIO[S, F[_]](implicit F: MonadIO[F]): MonadIO[RegionT[S, F, ?]] =
    new RegionTMonadIO[S, F](F)
}
trait RegionT1 extends RegionT2 {
  implicit def regionTMonadControlIO[S, F[_]](implicit F: MonadControlIO[F]): MonadControlIO[RegionT[S, F, ?]] =
    new RegionTMonadControlIO[S, F](F)
}
object RegionT extends RegionT1 {
  def apply[S, F[_], A](run: IORef[List[RefCountedFinalizer]] => F[A]): RegionT[S, F, A] =
    new RegionT(ReaderT(run))

  def liftIO[S, F[_], A](run: IORef[List[RefCountedFinalizer]] => IO[A])(implicit F: LiftIO[F]): RegionT[S, F, A] =
    RegionT[S, F, A](hs => F.liftIO[A](run(hs)))

  private[regions] def readerAE[F[_]]
  (implicit F: ApplicativeError[F, Throwable]): ApplicativeError[ReaderT[F, IORef[List[RefCountedFinalizer]], ?], Throwable] =
    implicitly[ApplicativeError[ReaderT[F, IORef[List[RefCountedFinalizer]], ?], Throwable]]
  private[regions] def readerM[F[_]]
  (implicit F: Monad[F]): Monad[ReaderT[F, IORef[List[RefCountedFinalizer]], ?]] =
    implicitly[Monad[ReaderT[F, IORef[List[RefCountedFinalizer]], ?]]]

  sealed class RegionTMonadError[S, F[_]](F: MonadError[F, Throwable]) extends MonadError[RegionT[S, F, ?], Throwable] {
    val M = readerM[F](F)
    val AE = readerAE[F](F)

    override def pure[A](x: A): RegionT[S, F, A] =
      RegionT[S, F, A](_ => F.pure(x))
    override def raiseError[A](e: Throwable): RegionT[S, F, A] =
      RegionT[S, F, A](_ => F.raiseError(e))
    override def flatMap[A, B](fa: RegionT[S, F, A])(f: A => RegionT[S, F, B]): RegionT[S, F, B] =
      new RegionT[S, F, B](M.flatMap(fa.run)(a => f(a).run))
    override def tailRecM[A, B](a: A)(f: A => RegionT[S, F, Either[A, B]]): RegionT[S, F, B] =
      new RegionT[S, F, B](M.tailRecM[A, B](a)(a => f(a).run))
    override def handleErrorWith[A](fa: RegionT[S, F, A])(f: Throwable => RegionT[S, F, A]): RegionT[S, F, A] =
      new RegionT[S, F, A](AE.handleErrorWith(fa.run)(a => f(a).run))
  }

  sealed class RegionTMonadIO[S, F[_]](F: MonadIO[F]) extends RegionTMonadError[S, F](F) with MonadIO[RegionT[S, F, ?]] {
    override def liftIO[A](a: IO[A]): RegionT[S, F, A] =
      RegionT[S, F, A](_ => F.liftIO(a))
  }

  sealed class RegionTMonadControlIO[S, F[_]](F: MonadControlIO[F]) extends RegionTMonadIO[S, F](F) with MonadControlIO[RegionT[S, F, ?]] {
    override def liftControl[A](cps: RunInBase[RegionT[S, F, ?], IO] => IO[A]): RegionT[S, F, A] =
      RegionT[S, F, A] { finalizersRef =>
        F.liftControl(runF => cps(new RunInBase[RegionT[S, F, ?], IO] {
          def apply[X](r: RegionT[S, F, X]): IO[RegionT[S, F, X]] = {
            val r1: IO[F[X]] = runF(r.run(finalizersRef))
            val r2: IO[RegionT[S, F, X]] = r1.map(x => RegionT[S, F, X](_ => x))
            r2
          }
        }))
      }
  }
}

object `package` {
  type ForallRegionT[F[_], A] = Forall[RegionT[?, F, A]]

  @SuppressWarnings(Array("org.wartremover.warts.NoNeedForMonad"))
  private[this] def addFinalizer[F[_]](finalizersRef: IORef[List[RefCountedFinalizer]], finalizer: IO[Unit]): IO[FinalizerHandle[F]] =
    for {
      countRef <- IORef.create[Int](1)
      h = RefCountedFinalizer(finalizer, countRef)
      _ <- finalizersRef.modify(h :: _)
    } yield FinalizerHandle[F](h)

  private[this] def exitBlock(finalizersRef: IORef[List[RefCountedFinalizer]]): IO[Unit] = for {
    fs <- finalizersRef.read
    _  <- fs.traverse_[IO, Unit](f => for {
        count <- f.refCount.modify(_ - 1)
        _     <- if (count == 0) f.run else IO.unit
      } yield ())
  } yield ()

  /**
    * Register a finalizer in the current region. When the region terminates,
    * all registered finalizers will be performed if they're not duplicated to a parent region.
    */
  def onExit[S, F[_] : LiftIO](finalizer: IO[Unit]): RegionT[S, F, FinalizerHandle[RegionT[S, F, ?]]] =
    RegionT.liftIO[S, F, FinalizerHandle[RegionT[S, F, ?]]] { finalizers =>
      addFinalizer[RegionT[S, F, ?]](finalizers, finalizer)
    }

  /**
    * Execute a region inside its parent region P. All resources which have been opened in the given
    * region and which haven't been duplicated using "dup", will be closed on exit from this function
    * whether by normal termination or by raising an exception.
    * Also all resources which have been duplicated to this region from a child region are closed
    * on exit if they haven't been duplicated themselves.
    * The Forall quantifier prevents resources from being returned by this function.
    */
  def runRegionT[P[_], A](r: ForallRegionT[P, A])(implicit P: MonadControlIO[P]): P[A] =
    IORef.create(List.empty[RefCountedFinalizer]).bracketIO(exitBlock)(r.apply.run.apply)
}