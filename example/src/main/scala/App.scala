import sio.core.{SafeApp, IO}
import sio.teletype._

object App extends SafeApp {
  def run(args: List[String]): IO[Unit] = List(
    "core"    -> core.run,
    "eff"     -> eff.run.runEff,
    "free"    -> free.main,
    "ioref"   -> ioref.run,
    "regions" -> regions.main,
    "st"      -> st.run,
    "maps"    -> maps.run
  ).foldLeft(IO.unit) { case (io, (name, main)) =>
      io >> putStrLn(s"Running $name") >> main >> putStrLn("")
  }
}
