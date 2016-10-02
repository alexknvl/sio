import cats.instances.list._
import cats.syntax.traverse._

object App {
  def main(args: Array[String]): Unit = List(
    core.run, eff.run.runEff, free.main
  ).sequence.map(x => ()).unsafeRun()
}
