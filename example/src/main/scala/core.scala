import sio.core.IO
import sio.teletype

object core {
  import teletype.{putStrLn, readLn}

  val getUserHome: IO[String] = IO { Option(System.getProperty("user.home")).get }
  def run = for {
    h <- getUserHome
    n <- readLn
    _ <- putStrLn(s"$h $n")
  } yield ()
}