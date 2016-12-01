package sio.core

import cats.~>
import sio.base.free.FreeME
import cats.syntax.either._

class RTS { self =>
  /**
    * This is the "back door" into the IO monad, allowing IO computation
    * to be performed at any time. For this to be safe, the IO computation
    * should be free of side effects and independent of its environment.
    *
    * @return a value of type `A`.
    */
  def run[A](io: IO[A]): Either[Throwable, A] =
    io.value.run(new (Op[World.Real, ?] ~> Either[Throwable, ?]) {
      override def apply[B](op: Op[World.Real, B]): Either[Throwable, B] = op match {
        case Op.Effect(f) => Either.catchNonFatal(f())
        case Op.ForkOS(fa) =>
          val thread = new Thread(new Runnable {
            override def run(): Unit = self.run(fa).fold(e => throw e, _ => ())
          })
          thread.start()
          Either.right(IO.mutable(thread))
      }
    })
}

object RTS {
  val defaultRTS = new RTS()
}