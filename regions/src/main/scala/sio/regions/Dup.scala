package sio.regions

import cats.data.ReaderT
import sio.core.{IORef, IO, MonadIO}
import sio.core.syntax.st._

/**
  * Duplicate a handle in the parent region.
  */
trait Dup[H[_[_]]] {
  def dup[F[_], CS, PS](h: H[RegionT[CS, RegionT[PS, F, ?], ?]])
  (implicit F: MonadIO[F]): RegionT[CS, RegionT[PS, F, ?], H[RegionT[PS, F, ?]]]
}


sealed abstract class DupInstances {
  import Dup._

  implicit val finalizerHandleDup: Dup[FinalizerHandle] =
    new Dup[FinalizerHandle] {
      type H[F[_]] = FinalizerHandle[F]
      type HR[S, F[_]] = H[RegionT[S, F, ?]]
      def H[F[_]](finalizer: RefCountedFinalizer): H[F] =
        FinalizerHandle[F](finalizer: RefCountedFinalizer)

      /**
        * Takes a handle in region `R` and moves it to some other region
        */
      def copy[S, F[_], R[_]](h: H[R])(implicit F: MonadIO[F]): RegionT[S, F, HR[S, F]] =
        RegionT.liftIO(hs =>
          h.finalizer.refCount.modify(_ + 1) *>
          hs.modify(h.finalizer :: _) *>
          IO.pure(H[RegionT[S, F, ?]](h.finalizer)))

      def dup[F[_], CS, PS](h: H[RegionT[CS, RegionT[PS, F, ?], ?]])
      (implicit F: MonadIO[F]): RegionT[CS, RegionT[PS, F, ?], HR[PS, F]] =
        RegionT[CS, RegionT[PS, F, ?], HR[PS, F]](
            hs => copy[PS, F, RegionT[CS, RegionT[PS, F, ?], ?]](h))
    }
}

object Dup extends DupInstances {
  /**Duplicates a handle to its parent region. */
  def dup[H[_[_]], F[_], CS, PS](h: H[RegionT[CS, RegionT[PS, F, ?], ?]])(implicit H: Dup[H], PP: MonadIO[F]):
    RegionT[CS, RegionT[PS, F, ?], H[RegionT[PS, F, ?]]] = H.dup(h)
}