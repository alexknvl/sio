package sio.core

import sio.base.free.FreeME

object Thunk {
  def unit[S]: Thunk[S, Unit] =
    FreeME.unit[Op[S, ?], Throwable]
  def pure[S, A](x: A): Thunk[S, A] =
    FreeME.pure[Op[S, ?], Throwable, A](x)
  def raiseError[S, A](e: Throwable): Thunk[S, A] =
    FreeME.raiseError[Op[S, ?], Throwable, A](e)
  def suspend[S, A](x: Op[S, A]): Thunk[S, A] =
    FreeME.suspend[Op[S, ?], Throwable, A](x)
}
