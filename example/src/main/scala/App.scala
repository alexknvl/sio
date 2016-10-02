import sio.core.IO
import sio.teletype._

object App {
  def main(args: Array[String]): Unit = Map(
    "core"  -> core.run,
    "eff"   -> eff.run.runEff,
    "free"  -> free.main,
    "ioref" -> ioref.run
  ).foldLeft(IO.unit) { case (io, (name, main)) =>
      io >> putStrLn(s"Running $name") >> main >> putStrLn("")
  }.unsafeRun()
}
