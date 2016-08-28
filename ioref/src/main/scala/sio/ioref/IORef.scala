package sio.ioref

import sio.core.IO

object `package` {
  def newIORef[A](a: => A): IO[IORef[A]] = IO.capture(new IORef[A] {
    override var value: A = a
  })
}

sealed abstract class IORef[A] {
  var value: A

  def read: IO[A] = IO.capture(value)
  def write(a: => A): IO[Unit] = IO.capture { value = a }
  def modify(f: A => A): IO[A] = IO.capture { value = f(value); value }
}
