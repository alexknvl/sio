import cats._
import cats.free.{Free, Inject}
import sio.core.IO
import sio.teletype

object free {
  sealed abstract class Interact[A] extends Product with Serializable
  object Interact {
    final case class Ask(message: String) extends Interact[String]
    final case class Tell(message: String) extends Interact[Unit]
  }

  class Interacts[F[_]](implicit F: Inject[Interact, F]) {
    def ask(message: String): Free[F, String] = Free.inject(Interact.Ask(message))
    def tell(message: String): Free[F, Unit] = Free.inject(Interact.Tell(message))
  }
  object Interacts {
    implicit def interacts[F[_]](implicit I: Inject[Interact, F]): Interacts[F] = new Interacts[F]
  }

  type App[A] = Interact[A]

  def program(implicit I: Interacts[App]): Free[App, Unit] = {
    import I._

    for {
      name <- ask("Hello, what's your name?")
      _    <- tell(s"Hey, $name")
    } yield ()
  }

  object InteractIOInterpreter extends (Interact ~> IO) {
    def apply[A](i: Interact[A]) = i match {
      case Interact.Ask(x) => teletype.putStrLn(x) >> teletype.readLn
      case Interact.Tell(x) => teletype.putStrLn(x)
    }
  }

  def main: IO[Unit] = program.foldMapUnsafe(InteractIOInterpreter)
}
