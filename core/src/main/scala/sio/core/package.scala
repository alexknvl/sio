package sio

import scala.reflect.ClassTag

package object core {
  final abstract class RealWorld private ()

  type IO[A] = ST[RealWorld, A]

  type STRef[S, A] = Ref[S, A]
  type IORef[A] = Ref[RealWorld, A]

  type IOMutable[A] = Mutable[RealWorld, A]

  type STArray[S, E] = Mutable[S, Array[E]]
  type IOArray[E] = IOMutable[Array[E]]

  type ForallST[A] = Forall[ST[?, A]]

  def newSTRef[S, A](a: A): ST[S, Ref[S, A]] =
    ST.unsafeCapture { new Ref[S, A](a) }
  def newIORef[A](a: => A): IO[IORef[A]] =
    IO { new Ref[RealWorld, A](a) }
  def fillSTArray[S, A](len: Int)(value: => A)(implicit A: ClassTag[A]): ST[S, STArray[S, A]] =
    ST.unsafeCapture { new STArray[S, A](Array.fill(len)(value)) }
}
