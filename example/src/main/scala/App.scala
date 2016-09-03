import sio.core.IO
import sio.eff.{EffectSet, Effect, EffIO}
import sio.teletype
import sio.ioref._

object App {
  final class ConsoleWrite private () extends Effect
  final class ConsoleRead private () extends Effect

  def putStrLn(s: String) =
    EffIO.lift1[ConsoleWrite, Unit](teletype.putStrLn(s))
  def readLn =
    EffIO.lift1[ConsoleRead, String](teletype.readLn)

  def run(args: List[String]) =
    readLn.flatMap(putStrLn).forever

  def main(args: Array[String]): Unit =
    run(args.toList).runEff.unsafeRun()
}
