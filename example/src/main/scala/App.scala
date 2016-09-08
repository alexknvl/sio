import java.nio.channels.ServerSocketChannel

import sio.core.IO
import sio.eff.{EffectSet, Effect, EffIO}
import sio.teletype
import sio.ioref._

import cats.instances.list._
import cats.syntax.traverse._

object IOApp {
  import teletype.{putStrLn, readLn}
  val getUserHome: IO[String] = IO { Option(System.getProperty("user.home")).get }
  def run = for {
    h <- getUserHome
    n <- readLn
    _ <- putStrLn(s"$h $n")
  } yield ()
}

object EffIOApp {
  final class ConsoleWrite private () extends Effect
  final class ConsoleRead private () extends Effect

  def putStrLn(s: String) =
    EffIO.lift1[ConsoleWrite, Unit](teletype.putStrLn(s))
  def readLn =
    EffIO.lift1[ConsoleRead, String](teletype.readLn)

  // The inferred type of run is EffIO[Effect.fx2[ConsoleRead, ConsoleWrite], Unit]
  def run = readLn.flatMap(putStrLn)
}

object App {
  def main(args: Array[String]): Unit = List(
    IOApp.run, EffIOApp.run.runEff
  ).sequence.map(x => ()).unsafeRun()
}
