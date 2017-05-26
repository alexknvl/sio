# sio

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

## Quick Start
```scala
resolvers += Resolver.bintrayRepo("alexknvl", "maven")

libraryDependencies ++= ((version: String) => List(
  // Core data types, including ST and IO
  "com.alexknvl"  %%  "sio-core"           % version,
  "com.alexknvl"  %%  "sio-concurrent"     % version,
  // Iteratees
  "com.alexknvl"  %%  "sio-iteratee"       % version,
  "com.alexknvl"  %%  "sio-iteratee-files" % version,
  "com.alexknvl"  %%  "sio-iteratee-gzip"  % version,
  // Monadic Regions  
  "com.alexknvl"  %%  "sio-regions"        % version,
  // Eff monad similar to PureScript
  "com.alexknvl"  %%  "sio-eff"            % version,
  // putStrLn etc
  "com.alexknvl"  %%  "sio-teletype"       % version))
  .apply("0.3.0")
```

## License
Code is provided under the MIT license available at https://opensource.org/licenses/MIT,
as well as in the LICENSE file.
