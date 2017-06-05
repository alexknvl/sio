import sio.core.{IORef, IO}
import sio.teletype._
import sio.core.syntax.io._

object ioref {
  def loop(ref: IORef[Int]): IO[Unit] = for {
    i   <- ref.read
    _   <- if (i < 3)
      putStrLn(s"$i") >> ref.write(i + 1) >> loop(ref)
    else IO.unit
  } yield ()

  def run = for {
    ref <- IORef.create(0)
    _   <- loop(ref)
    i   <- ref.read
    _   <- putStrLn(s"Done: $i")
  } yield ()
}