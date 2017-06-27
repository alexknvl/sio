import org.scalameter.api._
import sio.core._
import sio.core.syntax.all._
import fs2.Task

import scala.collection.mutable

object Sampler {
  private val rand = new java.util.Random()

  def iota(size: Int) : (Array[Int], Array[Int]) = {
    val set = mutable.Set.empty[Int]
    while (set.size < size) set += rand.nextInt(2 * size)

    val a = set.toArray.sorted
    val b = (0 until 2 * size).filter {!set.contains(_)}.toArray

    (a, b)
  }

  def rand(size: Int) : (Array[Int], Array[Int]) = {
    val set = mutable.Set.empty[Int]
    while (set.size < 2 * size) set += (rand.nextInt(size * 10) - size * 5)

    val v = set.toArray
    val a = v.slice(0, size).sorted
    val b = v.slice(size, 2 * size).sorted
    (a, b)
  }
}

object Benchmark extends Bench.LocalTime {
  val sizes = Gen.range("size")(25, 25, 5)
  val sizes1: Gen[Int] = Gen.exponential("size")(1024, 200000, 2)

  val arrays = for {
    size <- sizes1
  } yield Sampler.rand(size / 2)._1

  def pure(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else (pure(n - 1) + pure(n - 2)) % 10
  }
  def pureBig(n: Int): BigInt = {
    if (n == 0) BigInt(0)
    else if (n == 1) BigInt(1)
    else (pureBig(n - 1) + pureBig(n - 2)) % 10
  }
  def opt(n: Int): Option[Int] = {
    if (n == 0) Some(0)
    else if (n == 1) Some(1)
    else for {
      a <- opt(n - 1)
      b <- opt(n - 2)
    } yield (a + b) % 10
  }
  def fs2Task(n: Int): Task[Int] = {
    if (n == 0) Task.now(0)
    else if (n == 1) Task.now(1)
    else for {
      a <- fs2Task(n - 1)
      b <- fs2Task(n - 2)
    } yield (a + b) % 10
  }

  def io(n: Int): ST[RW, Int] = {
    if (n == 0) IO.pure(0)
    else if (n == 1) IO.pure(1)
    else for {
      a <- io(n - 1)
      b <- io(n - 2)
    } yield (a + b) % 10
  }

  def ioBig(n: Int): ST[RW, BigInt] = {
    if (n == 0) IO.pure(BigInt(0))
    else if (n == 1) IO.pure(BigInt(1))
    else for {
      a <- ioBig(n - 1)
      b <- ioBig(n - 2)
    } yield (a + b) % 10
  }

  def pureSum(arr: Array[Int]): Int =
    (0 until arr.length).foldLeft(0)((acc, i) => acc + arr(i))

  def stSum[S](arr: Mut[S, Array[Int]]): ST[S, Int] =
    (0 until arr.length).foldLeft(ST.pure[S, Int](0))((acc, i) => acc.flatMap(a => arr.get(i).map(_ + a)))

  performance of "Sum" in {
    measure method "pure" in {
      using (arrays) in { a =>
        pureSum(a)
      }
    }

    measure method "st" in {
      using (arrays) in { a =>
        IO.unsafeRun(stSum[RW](Mut.wrap[RW, Array[Int]](a)))
      }
    }
  }

  performance of "Benchmark" in {
    measure method "pure" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
        pure(a)
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

    measure method "pure + Big" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
        pureBig(a)
      }
    }

    measure method "IO + Big" in {
      using (sizes) config (exec.benchRuns -> 300, exec.minWarmupRuns -> 20000) in { a =>
        IO.unsafeRun(ioBig(a))
      }
    }
  }
}
