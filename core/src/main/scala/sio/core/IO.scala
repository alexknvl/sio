package sio.core

import cats.data.Xor
import sio.core.internal.TypeAligned

final case class IO[A](ops: TypeAligned[IO.Op, Unit, A]) {
  def map[B](f: A => B): IO[B] = IO(ops :+ IO.Op.Map(f))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(ops :+ IO.Op.Bind(f))
  def handleErrorWith(f: Throwable => IO[A]): IO[A] = IO(ops :+ IO.Op.Handle(f))

  def unsafeRun(): A = interpret.unsafePerformIO(this)
  def unsafeAttempt(): Throwable Xor A = interpret.unsafeAttemptIO(this)
}

object IO {
  def apply[A](f: => A): IO[A] = IO.capture(f)

  sealed abstract class Op[A, B] extends Product with Serializable
  object Op {
    final case class Map[A, B](f: A => B) extends Op[A, B]
    final case class Bind[A, B](f: A => IO[B]) extends Op[A, B]
    final case class Handle[A](f: Throwable => IO[A]) extends Op[A, A]
  }

  private[this] def make[A](a: Op[Unit, A]) = new IO(TypeAligned(a))

  def unit: IO[Unit] = IO(TypeAligned.empty[Op, Unit])
  def pure[A](x: A): IO[A] = make(Op.Map[Unit, A](_ => x))
  def capture[A](x: => A): IO[A] = make(Op.Map[Unit, A](_ => x))
  def raiseError[A](e: Throwable): IO[A] = make(Op.Map[Unit, A](_ => throw e))
}