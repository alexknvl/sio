package sio.core.detail

import cats.instances.either._
import cats.syntax.either._
import cats.{MonadError, ~>}

import scala.annotation.tailrec

final case class Thunk[F[_], E, A, B](ops: List[Op[F, E, Any, Any]]) {
  def :+[C](op: Op[F, E, B, C]): Thunk[F, E, A, C] =
    Thunk(op.asInstanceOf[Op[F, E, Any, Any]] :: ops)

  def map[C](f: B => C): Thunk[F, E, A, C] =
    this :+ Op.Map[F, E, B, C](f)
  def flatMap[C](f: B => Thunk[F, E, Unit, C]): Thunk[F, E, A, C] =
    this :+ Op.Bind[F, E, B, C](f)
  def handleErrorWith(f: E => Thunk[F, E, Unit, B]): Thunk[F, E, A, B] =
    this :+ Op.Handle[F, E, B](f)

  def run(value: Either[E, A], run: F ~> Either[E, ?]): Either[E, B] = {
    val M: MonadError[Either[E, ?], E] = MonadError[Either[E, ?], E]
    val anyUnit = ().asInstanceOf[Any]

    val queue = ArrayQueue.ofCapacity[Op[F, E, Any, Any]](ops.length)
    ops.foreach(queue.prepend)

    @tailrec def loop(value: Either[E, Any]): Either[E, Any] =
      queue.popHead() match {
        case None => value
        case Some(Op.Pure(x)) =>
          loop(x)
        case Some(Op.Suspend(x)) =>
          loop(M.flatMap(value)(_ => run.apply(x)))
        case Some(Op.Map(f)) =>
          loop(M.map(value)(f))
        case Some(Op.Bind(f)) =>
          loop(M.map(value) { v =>
            f(v).ops.foreach(queue.prepend)
            anyUnit
          })
        case Some(Op.Handle(f)) =>
          loop(M.handleError(value) { e =>
            f(e).ops.foreach(queue.prepend)
            anyUnit
          })
      }

    loop(value).map(_.asInstanceOf[B])
  }
}

object Thunk {
  def empty[F[_], E, A]: Thunk[F, E, A, A] =
    Thunk(Nil)
  def one[F[_], E, A, B](op: Op[F, E, A, B]): Thunk[F, E, A, B] =
    Thunk(List(op.asInstanceOf[Op[F, E, Any, Any]]))

  def unit[F[_], E]: Thunk[F, E, Unit, Unit] =
    empty
  def pure[F[_], E, A](x: A): Thunk[F, E, Unit, A] =
    one(Op.Pure[F, E, Unit, A](Right(x)))
  def suspend[F[_], E, A](x: F[A]): Thunk[F, E, Unit, A] =
    one(Op.Suspend[F, E, Unit, A](x))
  def raiseError[F[_], E, A](e: E): Thunk[F, E, Unit, A] =
    one(Op.Pure(Left(e)))
}