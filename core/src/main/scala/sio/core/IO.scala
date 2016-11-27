package sio.core

import sio.dmz.RealIO

object IO {
  val unit: IO[Unit] = ST.unit

  def apply[A](f: => A): IO[A] = new ST[RealWorld, A](RealIO.capture(f))

  def pure[A](x: A): IO[A] = ST.pure(x)
  def raiseError[A](e: Throwable): IO[A] = ST.raiseError(e)
  def trace(s: String): IO[Unit] = ST.trace(s)
}