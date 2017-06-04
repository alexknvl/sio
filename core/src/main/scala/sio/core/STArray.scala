package sio.core

import scala.reflect.ClassTag

object STArray {
  def fill[S, A](n: Int)(value: => A)(implicit A: ClassTag[A]): ST[S, STArray[S, A]] =
    ST.unsafeCapture { Mutable.wrap[S, Array[A]](Array.fill(n)(value)) }
  def tabulate[S, A](n: Int)(f: Int => A)(implicit A: ClassTag[A]): ST[S, STArray[S, A]] =
    ST.unsafeCapture { Mutable.wrap[S, Array[A]](Array.tabulate(n)(f)) }
}
