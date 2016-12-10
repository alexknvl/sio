package sio.core

final class Ref[S, A] private[core] (var unsafeValue: A) {
  def id: String = super.toString.dropWhile(_ != '@').tail

  def read: ST[S, A] = ST.unsafeCapture { unsafeValue }
  def write(a: A): ST[S, Unit] = ST.unsafeCapture { unsafeValue = a }
  def modify(f: A => A): ST[S, A] = ST.unsafeCapture { unsafeValue = f(unsafeValue); unsafeValue }

  override def toString: String = s"Ref[${unsafeValue.getClass.getSimpleName}]@$id"
}