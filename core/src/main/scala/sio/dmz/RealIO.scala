package sio.dmz

import scala.util.control.NonFatal
import cats.syntax.either._

final case class RealIO[A](thunk: Thunk) {
  def map[B](f: A => B): RealIO[B] =
    RealIO(thunk :+ Op.Map(Val.castK2(f)))
  def flatMap[B](f: A => RealIO[B]): RealIO[B] =
    RealIO(thunk :+ Op.Bind(Val.castK21(f andThen RealIO.getThunk)))
  def handleErrorWith(f: Throwable => RealIO[A]): RealIO[A] =
    RealIO(thunk :+ Op.Handle(f andThen RealIO.getThunk))

  def run(): A = RealIO.run(this)
  def attempt(): Either[Throwable, A] = RealIO.attempt(this)
}

object RealIO {
  val unit: RealIO[Unit] = RealIO(Vector.empty[Op])

  def pure[A](x: A): RealIO[A] = RealIO(Vector(Op.Map(Val.castK2((u: Unit) => x))))
  def capture[A](x: => A): RealIO[A] = RealIO(Vector(Op.Map(Val.castK2((u: Unit) => x))))
  def raiseError[A](e: Throwable): RealIO[A] = RealIO(Vector(Op.Map(Val.castK2((u: Unit) => (throw e) : Val))))

  def tailRecM[A, B](a: A)(f: A => RealIO[Either[A, B]]): RealIO[B] =
    pure(a).flatMap(a => f(a).flatMap {
      case Right(b) => pure(b)
      case Left(x) => tailRecM(x)(f)
    })

  def getThunk[A](io: RealIO[A]): Thunk = io.thunk

  def attempt[A](io: RealIO[A]): Either[Throwable, A] = {
    val buffer = io.thunk.toBuffer
    var value: Either[Throwable, Val] = Val.xorUnit

    while (buffer.nonEmpty) {
      try {
        while (buffer.nonEmpty) {
          val op = buffer.remove(0)
          op match {
            case Op.Map(f) =>
              value = value.map(f)
            case Op.Bind(f) =>
              value.map(f).foreach { thunk =>
                value = Val.xorUnit
                buffer.prependAll(thunk)
              }
            case Op.Handle(f) =>
              value.swap.map(f).foreach { thunk =>
                value = Val.xorUnit
                buffer.prependAll(thunk)
              }
          }
        }
      } catch {
        case NonFatal(e) => value = Either.left[Throwable, Val](e)
      }
    }

    value.map(Val.reify[A])
  }

  def run[A](io: RealIO[A]): A = attempt(io).fold(e => throw e, a => a)
}