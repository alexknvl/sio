package sio.regions

import cats.data.ReaderT
import sio.core.{IO, MonadIO}
import sio.ioref.IORef

/**Duplicate a handle in the parent region. */
trait Dup[H[_[_]]] {
  def dup[PP[_], CS, PS]
  (h: H[RegionT[CS, RegionT[PS, PP, ?], ?]])
  (implicit PP: MonadIO[PP]): RegionT[CS, RegionT[PS, PP, ?], H[RegionT[PS, PP, ?]]]
}


sealed abstract class DupInstances {
  import Dup._

  implicit val FinalizerHandleDup: Dup[FinalizerHandle] =
    new Dup[FinalizerHandle] {
      def dup[PP[_], CS, PS]
      (h: FinalizerHandle[RegionT[CS, RegionT[PS, PP, ?], ?]])
      (implicit PP: MonadIO[PP]): RegionT[CS, RegionT[PS, PP, ?], FinalizerHandle[RegionT[PS, PP, ?]]] = {
        RegionT[CS, RegionT[PS, PP, ?], FinalizerHandle[RegionT[PS, PP, ?]]](
          ReaderT[RegionT[PS, PP, ?], IORef[List[RefCountedFinalizer]], FinalizerHandle[RegionT[PS, PP, ?]]](
            hsIORef => copy[PS, PP, RegionT[CS, RegionT[PS, PP, ?], ?]](h)))
      }
    }
}

object Dup extends DupInstances {
  /**Duplicates a handle to its parent region. */
  def dup[H[_[_]], PP[_], CS, PS](h: H[RegionT[CS, RegionT[PS, PP, ?], ?]])(implicit H: Dup[H], PP: MonadIO[PP]):
    RegionT[CS, RegionT[PS, PP, ?], H[RegionT[PS, PP, ?]]] = H.dup(h)

  def copy[S, P[_], R[_]](handle: FinalizerHandle[R])(implicit P: MonadIO[P]): RegionT[S, P, FinalizerHandle[RegionT[S, P, ?]]] =
    RegionT[S, P, FinalizerHandle[RegionT[S, P, ?]]](ReaderT(finalizersRef => P.liftIO {
        handle.finalizer.refCount.modify(_ + 1) *>
          finalizersRef.modify(handle.finalizer :: _) *>
          IO.pure(FinalizerHandle[RegionT[S, P, ?]](handle.finalizer))
      }))
}