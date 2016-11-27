package sio

import scala.reflect.ClassTag

package object core {
  final abstract class RealWorld private ()

  type IO[A] = ST[RealWorld, A]

  type STRef[S, A] = Ref[S, A]
  type IORef[A] = Ref[RealWorld, A]

  type STHandle[S, A] = Handle[S, A]
  type IOHandle[A] = Handle[RealWorld, A]

  type STArray[S, E] = Handle[S, Array[E]]
  type IOArray[E] = IOHandle[Array[E]]

  def newSTRef[S, A](a: A): ST[S, Ref[S, A]] =
    ST.unsafeCapture { new Ref[S, A](a) }
  def newIORef[A](a: => A): IO[IORef[A]] =
    IO { new Ref[RealWorld, A](a) }
  def fillSTArray[S, A](len: Int)(value: => A)(implicit A: ClassTag[A]): ST[S, STArray[S, A]] =
    ST.unsafeCapture { new STArray[S, A](Array.fill(len)(value)) }
}
