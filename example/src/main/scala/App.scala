import sio.core._
import sio.teletype._
import sio.core.syntax.st._

object App extends SafeApp {
  def run(args: List[String]): ST[RW, Unit] = List(
    "core"      -> core.run,
    "eff"       -> eff.run.runEff,
    "free"      -> free.main,
    "ioref"     -> ioref.run,
    "regions"   -> regions.main,
    "st"        -> st.run,
    "maps"      -> maps.run,
    "callbacks" -> callbacks.run
  ).foldLeft(IO.unit) { case (io, (name, main)) =>
      io >> putStrLn(s"Running $name") >> main >> putStrLn("")
  } >> putStrLn(IO.unit.getClass.toString)
}
