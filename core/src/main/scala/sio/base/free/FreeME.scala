package sio.base.free

import cats.{~>, MonadError}
import cats.instances.either._
import cats.syntax.either._
import sio.base.free.FreeME._
import sio.base.free.detail._

import scala.annotation.tailrec

final case class FreeME[F[_], E, A](unsafeThunk: Vector[Op[F, E]]) {
  def map[B](f: A => B): FreeME[F, E, B] =
    FreeME(unsafeThunk :+ Op.Map(Val.castK2(f)))
  def flatMap[B](f: A => FreeME[F, E, B]): FreeME[F, E, B] =
    FreeME(unsafeThunk :+ Op.Bind(Val.castK21(f andThen (_.unsafeThunk))))
  def handleErrorWith[B](f: E => FreeME[F, E, B]): FreeME[F, E, B] =
    FreeME(unsafeThunk :+ Op.Handle(f andThen (_.unsafeThunk)))

  def run(run: F ~> Either[E, ?]): Either[E, A] ={
    val queue = ArrayQueue.fromArray(unsafeThunk.toArray)
    val M: MonadError[Either[E, ?], E] = MonadError[Either[E, ?], E]

    @tailrec def loop(value: Either[E, Val]): Either[E, Val] =
      queue.popFront() match {
        case None => value
        case Some(Op.Pure(x)) => loop(x)
        case Some(Op.Suspend(x)) => loop(M.flatMap(value)(_ => run.apply(x)))
        case Some(Op.Map(f)) => loop(M.map(value)(f))
        case Some(Op.Bind(f)) =>
          loop(M.flatMap(value) { v =>
            queue.prependAll(f(v))
            Either.right(Val.unit)
          })
        case Some(Op.Handle(f)) =>
          loop(M.handleErrorWith(value) { e =>
            queue.prependAll(f(e))
            Either.right(Val.unit)
          })
      }

    loop(Either.right(Val.unit)).map(Val.reify[A])
  }
}

object FreeME {
  sealed abstract class Op[F[_], E] extends Product with Serializable
  object Op {
    final case class Pure[F[_], E](value: Either[E, Val]) extends Op[F, E]
    final case class Suspend[F[_], E](f: F[Val]) extends Op[F, E]

    final case class Map[F[_], E](f: Val => Val) extends Op[F, E]
    final case class Bind[F[_], E](f: Val => Vector[Op[F, E]]) extends Op[F, E]
    final case class Handle[F[_], E](f: E => Vector[Op[F, E]]) extends Op[F, E]
  }

  def unit[F[_], E]: FreeME[F, E, Unit] =
    FreeME(Vector.empty[Op[F, E]])
  def pure[F[_], E, A](x: A): FreeME[F, E, A] =
    FreeME(Vector(Op.Pure(Right(Val.cast(x)))))
  def suspend[F[_], E, A](x: F[A]): FreeME[F, E, A] =
    FreeME(Vector(Op.Suspend[F, E](Val.castK1(x))))
  def raiseError[F[_], E, A](e: E): FreeME[F, E, A] =
    FreeME(Vector(Op.Pure(Left(e))))

  def instance[F[_], E]: MonadError[FreeME[F, E, ?], E] = new MonadError[FreeME[F, E, ?], E] {
    override def pure[A](x: A): FreeME[F, E, A] =
      FreeME.pure(x)
    override def raiseError[A](e: E): FreeME[F, E, A] =
      FreeME.raiseError(e)

    override def map[A, B](fa: FreeME[F, E, A])(f: A => B): FreeME[F, E, B] =
      fa.map(f)
    override def flatMap[A, B](fa: FreeME[F, E, A])(f: (A) => FreeME[F, E, B]): FreeME[F, E, B] =
      fa.flatMap(f)
    override def handleErrorWith[A](fa: FreeME[F, E, A])(f: E => FreeME[F, E, A]): FreeME[F, E, A] =
      fa.handleErrorWith(f)

    override def tailRecM[A, B](a: A)(f: A => FreeME[F, E, Either[A, B]]): FreeME[F, E, B] =
      pure(a).flatMap(a => f(a).flatMap {
        case Right(b) => pure(b)
        case Left(x) => tailRecM(x)(f)
      })
  }
}