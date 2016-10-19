package sio.ioref

import sio.core.IO

object `package` {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def newIORef[A](a: => A): IO[IORef[A]] = IO(new IORef[A] {
    override var value: A = a
  })
}

sealed abstract class IORef[A] {
  var value: A

  def read: IO[A] = IO { value }
  def write(a: => A): IO[Unit] = IO { value = a }
  def modify(f: A => A): IO[A] = IO { value = f(value); value }

  override def toString: String = {
    val addr = super.toString.dropWhile(_ != '@').tail
    s"IORef[$addr, $value]"
  }
}
