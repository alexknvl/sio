package sio.regions

import sio.core.IO
import sio.ioref.IORef

/**
  * Created by alex on 9/3/16.
  */
final case class RefCountedFinalizer(run: IO[Unit], refCount: IORef[Int])
