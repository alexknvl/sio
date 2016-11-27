import sio.core._

import scala.reflect.ClassTag
import sio.teletype.putStrLn

object st {
  final class JArray[S, A](value: Array[A]) extends Mutable[S, Array[A]] {
    def length: ST[S, Int] = capture { value.length }
    def get(i: Int): ST[S, A] = capture { value(i) }
    def set(i: Int, x: A): ST[S, Unit] = capture { value(i) = x }
    def map(f: A => A): ST[S, Unit] = capture { for (i <- value.indices) value(i) = f(value(i)) }
  }
  object JArray {
    def fill[S, A](len: Int)(value: => A)(implicit A: ClassTag[A]): ST[S, JArray[S, A]] =
      ST.unsafeCapture { new JArray[S, A](Array.fill(len)(value)) }
  }

  def calculate[S]: ST[S, Int] = for {
    a <- JArray.fill(10)(2)
    _ <- a.map(_ * 2)
    _ <- a.set(3, 0)
    x <- a.get(3)
    y <- a.get(1)
  } yield x + y

  def run: IO[Unit] = {
    val x = ST.attempt(new Forall[ST[?, Int]] { def apply[A]: ST[A, Int] = calculate })
    putStrLn(x.toString)
  }
}
