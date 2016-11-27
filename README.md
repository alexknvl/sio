# sio

[![Join the chat at https://gitter.im/scala-io/Lobby](https://badges.gitter.im/scala-io/Lobby.svg)](https://gitter.im/scala-io/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Scala IO monad

```scala
import sio.core.IO
import sio.teletype._

object App {
  val getUserHome: IO[String] = IO { Option(System.getProperty("user.home")).get }
  def pureMain = for {
    h <- getUserHome
    _ <- putStr("What's your name? ")
    n <- readLn
    _ <- putStrLn(s"Hi, $n, your home directory is $h!")
  } yield ()
  
  def main(args: Array[String]): Unit = pureMain.unsafeRun()
}
```

See more examples [here](https://github.com/alexknvl/sio/tree/master/example/src/main/scala).

