package sio.core

abstract class STMutable[S, A] protected (protected val value: A) {
  protected def unsafe[F](f: => F): ST[S, F] = ST { dmz.capture(f) }

  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"STMutable[$id]"
}

