import org.scalameter.api._
import sio.core.IO
import sio.core.syntax.io._
import fs2.Task
import sio.base.free.`package`.RealIO

object Benchmark extends Bench.LocalTime {
  val sizes = Gen.range("size")(25, 25, 5)

  def pure(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else (pure(n - 1) + pure(n - 2)) % 10
  }

  final case class Wrapped(value: Int) extends AnyVal {
    def +(that: Wrapped): Wrapped = Wrapped(this.value + that.value)
    def %(that: Int): Wrapped = Wrapped(this.value % that)
  }

  def wrapped(n: Int): Wrapped = {
    if (n == 0) Wrapped(0)
    else if (n == 1) Wrapped(1)
    else (wrapped(n - 1) + wrapped(n - 2)) % 10
  }

  def opt(n: Int): Option[Int] = {
    if (n == 0) Some(0)
    else if (n == 1) Some(1)
    else for {
      a <- opt(n - 1)
      b <- opt(n - 2)
    } yield (a + b) % 10
  }

  def io(n: Int): IO[Int] = {
    if (n == 0) IO.pure(0)
    else if (n == 1) IO.pure(1)
    else for {
      a <- io(n - 1)
      b <- io(n - 2)
    } yield (a + b) % 10
  }

  def realIO(n: Int): RealIO[Int] = {
    if (n == 0) IO.pure(0)
    else if (n == 1) IO.pure(1)
    else RealIO.flatMap(realIO(n - 1)) { a =>
      RealIO.map(realIO(n - 2)) { b =>
        (a + b) % 10
      }
    }
  }

  def fs2Task(n: Int): Task[Int] = {
    if (n == 0) Task.now(0)
    else if (n == 1) Task.now(1)
    else for {
      a <- fs2Task(n - 1)
      b <- fs2Task(n - 2)
    } yield (a + b) % 10
  }

  performance of "Benchmark" in {
    measure method "pure" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
        pure(a)
      }
    }

    measure method "wrapped" in {
      using (sizes) in { a =>
        wrapped(a)
      }
    }

    measure method "Option" in {
      using (sizes) in { a =>
        opt(a)
      }
    }

    measure method "IO" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
        IO.unsafeRun(io(a))
      }
    }

    measure method "RealIO" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
        IO.unsafeRun(io(a))
      }
    }

    measure method "fs2.Task" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
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
