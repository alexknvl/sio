package sio.base.free

import cats.{Id, ~>, Monad}
import sio.base.free.detail._

import scala.annotation.tailrec

final case class Free[F[_], A](unsafeThunk: Vector[Free.Op[F]]) {
  def map[B](f: A => B): Free[F, B] =
    Free(unsafeThunk :+ Free.Op.Map(Val.castK2(f)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    Free(unsafeThunk :+ Free.Op.Bind(Val.castK21(f andThen (_.unsafeThunk))))

  def run(run: F ~> Id): A = {
      val queue = ArrayQueue.fromArray(unsafeThunk.toArray)

      @tailrec def loop(value: Val): Val =
        queue.popFront() match {
          case None => value
          case Some(Free.Op.Pure(x)) => loop(x)
          case Some(Free.Op.Suspend(x)) => loop(run.apply(x))
          case Some(Free.Op.Map(f)) => loop(f(value))
          case Some(Free.Op.Bind(f)) => loop({ queue.prependAll(f(value)); Val.unit })
        }

      Val.reify[A](loop(Val.unit))
  }
}

object Free {
  sealed abstract class Op[F[_]] extends Product with Serializable
  object Op {
    final case class Pure[F[_]](value: Val) extends Op[F]
    final case class Suspend[F[_]](f: F[Val]) extends Op[F]

    final case class Map[F[_]](f: Val => Val) extends Op[F]
    final case class Bind[F[_]](f: Val => Vector[Op[F]]) extends Op[F]
  }

  def unit[F[_]]: Free[F, Unit] = Free(Vector.empty[Op[F]])
  def pure[F[_], A](x: A): Free[F, A] = Free(Vector(Op.Pure(Val.cast(x))))
  def suspend[F[_], A](x: F[A]): Free[F, A] = Free(Vector(Op.Suspend[F](Val.castK1(x))))

  def instance[F[_]]: Monad[Free[F, ?]] = new Monad[Free[F, ?]] {
    override def pure[A](x: A): Free[F, A] = Free.pure(x)
    override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.map(f)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
      pure(a).flatMap(a => f(a).flatMap {
        case Right(b) => pure(b)
        case Left(x) => tailRecM(x)(f)
      })
  }
}