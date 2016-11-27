import sio.core._

import scala.reflect.ClassTag
import sio.teletype.putStrLn

object st {
  type JArray[S, E] = Handle[S, Array[E]]

  implicit class ArraySyntax[S, E](handle: JArray[S, E]) extends Handle.Syntax(handle) {
    def length: ST[S, Int] = lift { _.length }
    def get(i: Int): ST[S, E] = lift { _.apply(i) }
    def set(i: Int, x: E): ST[S, Unit] = lift { _.update(i, x) }
    def map(f: E => E): ST[S, Unit] = lift { a => for (i <- a.indices) a(i) = f(a(i)) }
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
