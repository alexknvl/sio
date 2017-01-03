import cats.syntax.all._
import cats.instances.list._

import sio.core.IO
import sio.core.instances.all._
import sio.teletype.putStrLn

import scala.util.Random

object maps {
  def random(k: Int): IO[Int] = IO { Random.nextInt(k) }

  def sum(start: Int, n: Int): IO[Int] =
    (0 until n).foldLeft(IO.pure(start))((acc, _) => acc.map(_ + 1))

  def run: IO[Unit] = for {
    ns <- (0 until 25).toList.map(n => random(n * 5 + 1)).sequence
    x <- ns.foldLeft(IO.pure(0))((acc, n) => acc.flatMap(s => sum(n, s)))
    _ <- putStrLn(s"${ns.sum} == $x")
  } yield ()
}
