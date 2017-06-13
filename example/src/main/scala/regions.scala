import sio.core._
import sio.teletype._
import sio.regions._
import sio.regions.Dup._

import cats.syntax.all._
import sio.core.syntax.st._
import sio.core.instances.all._

object regions {
  def putStrLn_[F[_]](s: String)(implicit F: LiftIO[F]): F[Unit] = F.liftIO(putStrLn(s))

  def openResource[S, F[_]](path: String)(implicit F: MonadIO[F]): RegionT[S, F, FinalizerHandle[RegionT[S, F, ?]]] = for {
    _ <- putStrLn_[RegionT[S, F, ?]](s"Opening $path")
    f <- onExit[S, F](putStrLn(s"Closing $path."))
  } yield f

  def nested[S, F[_]](implicit F: MonadIO[F]): RegionT[S, F, FinalizerHandle[RegionT[S, F, ?]]] = for {
    _ <- onExit[S, F](putStrLn("Inner scope is closed."))
    c <- openResource[S, F]("config1")
    f <- openResource[S, F]("config2")
  } yield f

  def regionMain[S, F[_]](implicit F: MonadControlIO[F]): RegionT[S, F, Unit] = for {
    _ <- onExit[S, F](putStrLn("Outer scope is closed."))
    f <- runRegionT(new ForallRegionT[RegionT[S, F, ?], FinalizerHandle[RegionT[S, F, ?]]] {
      def apply[CS] = for {
        b <- nested[CS, RegionT[S, F, ?]]
        h <- dup(b)
      } yield h
    })
    _ <- putStrLn_[RegionT[S, F, ?]](s"Finishing up in the outer scope.")
  } yield ()

  def main: IO[Unit] = runRegionT(new ForallRegionT[IO, Unit] { def apply[A] = regionMain })
}