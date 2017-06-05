import org.scalameter.api._
import sio.core.IO
import sio.core.syntax.io._

import fs2.Task

object Benchmark extends Bench.LocalTime {
  val sizes = Gen.range("size")(30, 30, 5)

  def pureScala(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else (pureScala(n - 1) + pureScala(n - 2)) % 10
  }

  def io(n: Int): IO[Int] = {
    if (n == 0) IO.pure(0)
    else if (n == 1) IO.pure(1)
    else for {
      a <- io(n - 1)
      b <- io(n - 2)
    } yield (a + b) % 10
  }

  def fs2Task(n: Int): Task[Int] = {
    if (n == 0) Task.delay(0)
    else if (n == 1) Task.delay(1)
    else for {
      a <- fs2Task(n - 1)
      b <- fs2Task(n - 2)
    } yield (a + b) % 10
  }

  performance of "IO" in {
    measure method "pure" in {
      using (sizes) in { a =>
        pureScala(a)
      }
    }

    measure method "sio.core.IO" in {
      using (sizes) in { a =>
        IO.unsafeRun(io(a))
      }
    }

    measure method "fs2.Task" in {
      using (sizes) in { a =>
        implicit val s: fs2.Strategy = fs2.Strategy.sequential
        fs2Task(a).unsafeRun()
      }
    }

//    measure method "findFast" in {
//      using (arrays) config (
//        exec.benchRuns -> 10000
//        ) in { case (a, b) =>
//        FindNth.findNth(a, b)
//      }
//    }
  }
}
