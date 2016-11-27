import sio.core._
import sio.core.syntax.all._
import sio.teletype.putStrLn
import cats.kernel.instances.int._

object st {
  def calculate[S]: ST[S, (Int, Int)] = for {
    a <- fillSTArray(10)(2)
    _ <- a.transform(_ * 2)
    _ <- a.set(3, 0)
    _ <- a.stableSort
    first <- a.get(0)
    second <- a.get(1)
  } yield (first, second)

  def run: IO[Unit] = {
    val x = ST.attempt(new Forall[ST[?, (Int, Int)]] { def apply[A]: ST[A, (Int, Int)] = calculate })
    putStrLn(x.toString)
  }
}
