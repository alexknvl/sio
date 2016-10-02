import sio.core.IO
import sio.teletype._
import sio.ioref._

object ioref {
  def loop(ref: IORef[Int]): IO[Unit] = for {
    i   <- ref.read
    _   <- if (i < 3)
      putStrLn(s"$i") >> ref.write(i + 1) >> loop(ref)
    else IO.unit
  } yield ()

  def run = for {
    ref <- newIORef(0)
    _   <- loop(ref)
    i   <- ref.read
    _   <- putStrLn(s"Done: $i")
  } yield ()
}