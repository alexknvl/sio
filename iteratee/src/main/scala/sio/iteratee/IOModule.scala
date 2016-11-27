package sio.iteratee

import cats.MonadError
import sio.core.IO
import sio.core.instances.all._
import io.iteratee.{IterateeErrorModule, EnumeratorErrorModule, EnumerateeModule, Module}

trait IOModule extends Module[IO]
  with EnumerateeModule[IO]
  with EnumeratorErrorModule[IO, Throwable]
  with IterateeErrorModule[IO, Throwable]
{
  final type M[f[_]] = MonadError[f, Throwable]
  final protected val F: MonadError[IO, Throwable] = MonadError[IO, Throwable]
  final protected def captureEffect[A](a: => A): IO[A] = IO.apply(a)
}

object `package` extends IOModule