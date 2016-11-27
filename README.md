# sio

[![Join the chat at https://gitter.im/scala-io/Lobby](https://badges.gitter.im/scala-io/Lobby.svg)](https://gitter.im/scala-io/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Scala IO monad

```scala
import sio.core.{ IO, SafeApp }
import sio.teletype._

object App extends SafeApp {
  val getUserHome: IO[String] = IO { Option(System.getProperty("user.home")).get }

  def run(args: List[String]): IO[Unit] = for {
    h <- getUserHome
    _ <- putStr("What's your name? ")
    n <- getLine
    _ <- putStrLn(s"Hi, $n, your home directory is $h!")
  } yield ()
}
```

See more examples [here](https://github.com/alexknvl/sio/tree/master/example/src/main/scala).

