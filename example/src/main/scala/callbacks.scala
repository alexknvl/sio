import sio.core._
import sio.teletype._
import sio.core.syntax.io._

object callbacks {
  def func(f: () => Unit): Impure[Unit] = {
    (0 until 3).foreach(_ => f())
  }

  def run = for {
    ref <- IORef.create(0)
    cb  <- ref.modify(_ + 1).map(_ => ()).asCallback
    _   <- IO { func(cb) }
    i   <- ref.read
    _   <- putStrLn(s"Done: $i")
  } yield ()
}
