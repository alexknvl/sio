package sio.iteratee.files

import sio.core.IO
import io.iteratee.files.SuspendableFileModule
import sio.iteratee.IOModule

trait IOFileModule extends IOModule with SuspendableFileModule[IO]

object `package` extends IOFileModule