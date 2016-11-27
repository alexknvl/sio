package sio.core

abstract class Mutable[S, A] {
  protected def capture[F](f: => F): ST[S, F] = ST { dmz.capture(f) }

  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"STMutable[$id]"
}

