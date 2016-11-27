package sio.core

final class Ref[S, A] private[core] (var unsafeValue: A) extends Mutable[S, A] {
  def read: ST[S, A] = capture { unsafeValue }
  def write(a: => A): ST[S, Unit] = capture { unsafeValue = a }
  def modify(f: A => A): ST[S, A] = capture { unsafeValue = f(unsafeValue); unsafeValue }

  override def toString: String = s"Ref[$id]"
}