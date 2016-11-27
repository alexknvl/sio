package sio.core

sealed abstract class STRef[S, A] {
  def read: ST[S, A]
  def write(a: => A): ST[S, Unit]
  def modify(f: A => A): ST[S, A]

  def id: String = super.toString.dropWhile(_ != '@').tail
  override def toString: String = s"STRef[$id]"
}

object STRef {
  def create[S, A](a: A): ST[S, STRef[S, A]] = ST(dmz.capture {
    new STRef[S, A] {
      var unsafeValue = a
      def read: ST[S, A] = ST(dmz.capture { unsafeValue })
      def write(a: => A): ST[S, Unit] = ST(dmz.capture { unsafeValue = a })
      def modify(f: A => A): ST[S, A] = ST(dmz.capture { unsafeValue = f(unsafeValue); unsafeValue })
    }
  })
}