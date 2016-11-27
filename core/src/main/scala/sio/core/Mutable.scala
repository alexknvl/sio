package sio.core

import sio.core.dmz.RealIO

abstract class Mutable[S, A] {
  protected def capture[F](f: => F): ST[S, F] = ST { RealIO.capture(f) }

  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"STMutable[$id]"
}

