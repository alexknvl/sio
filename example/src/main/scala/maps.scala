import cats.syntax.all._
import cats.instances.list._

import sio.core._
import sio.core.instances.all._
import sio.core.syntax.st._
import sio.teletype.putStrLn

import scala.util.Random

object maps {
  def random(k: Int): ST[RW, Int] = IO { Random.nextInt(k) }

  def sum(start: Int, n: Int): ST[RW, Int] =
    (0 until n).foldLeft(IO.pure(start))((acc, _) => acc.map(_ + 1))

  def run: ST[RW, Unit] = for {
    ns <- (0 until 25).toList.map(n => random(n * 5 + 1)).sequence
    x <- ns.foldLeft(IO.pure(0)) { (acc: ST[RW, Int], n: Int) => acc.flatMap(s => sum(n, s)) }
    _ <- putStrLn(s"${ns.sum} == $x")
  } yield ()
}
