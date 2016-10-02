import sio.eff.{EffIO, Effect}
import sio.teletype

object eff {
  final class ConsoleWrite private () extends Effect
  final class ConsoleRead private () extends Effect

  def putStrLn(s: String) =
    EffIO.lift1[ConsoleWrite, Unit](teletype.putStrLn(s))
  def readLn =
    EffIO.lift1[ConsoleRead, String](teletype.readLn)

  // The inferred type of run is EffIO[Effect.fx2[ConsoleRead, ConsoleWrite], Unit]
  def run = readLn.flatMap(putStrLn)
}