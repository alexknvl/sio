package sio.iteratee

import cats.MonadError
import sio.core.{MonadIO, IO}
import io.iteratee.{IterateeErrorModule, EnumeratorErrorModule, EnumerateeModule, Module}

trait IOModule extends Module[IO]
  with EnumerateeModule[IO]
  with EnumeratorErrorModule[IO, Throwable]
  with IterateeErrorModule[IO, Throwable]
{
  final type M[f[_]] = MonadError[f, Throwable]
  final protected val F: MonadError[IO, Throwable] = IO.instance
  final protected def captureEffect[A](a: => A): IO[A] = IO.apply(a)
}

object `package` extends IOModule