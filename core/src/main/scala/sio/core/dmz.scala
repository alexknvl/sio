package sio.core

import cats.data.{EitherT, Xor}
import cats.syntax.either._
import scala.annotation.tailrec
import scala.util.control.NonFatal

@SuppressWarnings(Array(
  "org.wartremover.warts.AsInstanceOf",
  "org.wartremover.warts.IsInstanceOf",
  "org.wartremover.warts.Throw",
  "org.wartremover.warts.While",
  "org.wartremover.warts.Var",
  "org.wartremover.warts.MutableDataStructures"))
object dmz {
  type Val = Any with ({ type Tag = Any })
  object Val {
    val unit = cast(())
    val xorUnit = Either.right[Throwable, Val](Val.unit)

    def cast[A](a: A): Val = a.asInstanceOf[Val]
    def castK2[F[_, _], A, B](a: F[A, B]): F[Val, Val] = a.asInstanceOf[F[Val, Val]]
    def castK21[F[_, _], A, B](a: F[A, B]): F[Val, B] = a.asInstanceOf[F[Val, B]]
    def castK22[F[_, _], A, B](a: F[A, B]): F[A, Val] = a.asInstanceOf[F[A, Val]]

    def reify[A](a: Val): A = a.asInstanceOf[A]
  }

  type Thunk = Vector[Op]
  object Thunk {
    def empty: Thunk = Vector.empty[Op]
  }

  sealed abstract class Op extends Product with Serializable
  object Op {
    final case class Map(f: Val => Val) extends Op
    final case class Bind(f: Val => Thunk) extends Op
    final case class Handle(f: Throwable => Thunk) extends Op
  }

  val unit: RealIO[Unit] = RealIO(Vector.empty[Op])

  def pure[A](x: A): RealIO[A] =
    RealIO(Vector(Op.Map(Val.castK2((u: Unit) => x))))
  def capture[A](x: => A): RealIO[A] =
    RealIO(Vector(Op.Map(Val.castK2((u: Unit) => x))))
  def raiseError[A](e: Throwable): RealIO[A] =
    RealIO(Vector(Op.Map(Val.castK2((u: Unit) => (throw e) : Val))))

  def tailRecM[A, B](a: A)(f: A => RealIO[Either[A, B]]): RealIO[B] =
    pure(a).flatMap(a => f(a).flatMap {
      case Right(b) => pure(b)
      case Left(x) => tailRecM(x)(f)
    })

  def getThunk[A](io: RealIO[A]): Thunk = io.thunk

  final case class RealIO[A](thunk: Thunk) {
    def map[B](f: A => B): RealIO[B] =
      RealIO(thunk :+ Op.Map(Val.castK2(f)))
    def flatMap[B](f: A => RealIO[B]): RealIO[B] =
      RealIO(thunk :+ Op.Bind(Val.castK21(f andThen getThunk)))
    def handleErrorWith(f: Throwable => RealIO[A]): RealIO[A] =
      RealIO(thunk :+ Op.Handle(f andThen getThunk))

    def run(): A = unsafePerformIO(this)
    def attempt(): Either[Throwable, A] = unsafeAttemptIO(this)
  }

  def unsafeAttemptIO[A](io: RealIO[A]): Either[Throwable, A] = {
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

  def unsafePerformIO[A](io: RealIO[A]): A =
    unsafeAttemptIO(io).fold(e => throw e, a => a)
}
