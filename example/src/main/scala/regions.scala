import sio.core.{IO, Forall}
import sio.teletype._
import sio.regions._
import cats.syntax.all._
import sio.core.instances.all._

object regions {
  def regionMain[S]: RegionT[S, IO, Unit] = for {
    _ <- putStrLn("Opening a file.").liftIO[RegionT[S, IO, ?]]
    _ <- onExit[S, IO](putStrLn("Closing the file."))
  } yield ()

  def main: IO[Unit] = runRegionT(new Forall[RegionT[?, IO, Unit]] { def apply[A] = regionMain })
}