package sio.core

import cats.data.Xor
import scala.collection.mutable
import scala.util.Try
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
    val xorUnit = Xor.right[Throwable, Val](Val.unit)

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

  val unit: IO[Unit] = RealIO(Vector.empty[Op])

  def pure[A](x: A): IO[A] =
    RealIO(Vector(Op.Map(Val.castK2((u: Unit) => x))))
  def capture[A](x: => A): IO[A] =
    RealIO(Vector(Op.Map(Val.castK2((u: Unit) => x))))
  def raiseError[A](e: Throwable): IO[A] =
    RealIO(Vector(Op.Map(Val.castK2((u: Unit) => (throw e) : Val))))

  def getIOThunk[A](io: IO[A]): Thunk =
    io.asInstanceOf[RealIO[A]].thunk

  final case class RealIO[A](thunk: Thunk) extends ForwarderIO[A] {
    def map[B](f: A => B): IO[B] =
      RealIO(thunk :+ Op.Map(Val.castK2(f)))
    def flatMap[B](f: A => IO[B]): IO[B] =
      RealIO(thunk :+ Op.Bind(Val.castK21(f andThen getIOThunk)))
    def handleErrorWith(f: Throwable => IO[A]): IO[A] =
      RealIO(thunk :+ Op.Handle(f andThen getIOThunk))

    override def unsafeAttempt(): Xor[Throwable, A] = unsafeAttemptIO(this)
    override def unsafeRun(): A = unsafePerformIO(this)
  }

  def unsafeAttemptIO[A](io: RealIO[A]): Xor[Throwable, A] = {
    val buffer = io.thunk.toBuffer
    var value: Xor[Throwable, Val] = Val.xorUnit

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
        case NonFatal(e) => value = Xor.left[Throwable, Val](e)
      }
    }

    value.map(Val.reify[A])
  }

  def unsafePerformIO[A](io: RealIO[A]): A =
    unsafeAttemptIO(io).fold(e => throw e, a => a)
}
