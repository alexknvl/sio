import sio.core.IO
import sio.teletype._
import sio.core.syntax.io._

object core {
  val getUserHome: IO[String] = IO { Option(System.getProperty("user.home")).get }
  def run = for {
    h <- getUserHome
    _ <- putStr("What's your name? ")
    n <- getLine
    _ <- putStrLn(s"Hi, $n, your home directory is $h!")
  } yield ()
}