package sio.base.free

import cats._
import sio.base.RelMonad
import sio.base.data.TASeq
import cats.syntax.either._

sealed abstract class FreeRM[I[_], F[_], A] extends Product with Serializable {
  def map[B](f: A => B): FreeRM[I, F, B]
  def bind[B](f: I[A] => FreeRM[I, F, B]): FreeRM[I, F, B]

  def step(run: F ~> I)(implicit I: Monad[I]): Either[FreeRM[I, F, A], I[A]]
  def go(run: F ~> I)(implicit I: Monad[I]): I[A]
}
object FreeRM {
  sealed abstract class Op[I[_], F[_], A, B] extends Product with Serializable
  object Op {
    final case class Map[I[_], F[_], A, B](f: A => B) extends Op[I, F, A, B]
    final case class Bind[I[_], F[_], A, B](f: I[A] => FreeRM[I, F, B]) extends Op[I, F, A, B]
  }

  final case class View[I[_], F[_], A, B]
  (value: Either[F[A], I[A]], ops: TASeq[Op[I, F, ?, ?], A, B])
    extends FreeRM[I, F, B]
  {
    override def map[C](f: B => C): FreeRM[I, F, C] =
      View[I, F, A, C](value, ops :+ Op.Map[I, F, B, C](f))
    override def bind[C](f: I[B] => FreeRM[I, F, C]): FreeRM[I, F, C] =
      View[I, F, A, C](value, ops :+ Op.Bind[I, F, B, C](f))

    override def step(run: F ~> I)(implicit I: Monad[I]): Either[FreeRM[I, F, B], I[B]] =
      (value.fold(run.apply[A], identity), ops.uncons) match {
        case (x, Left(ab)) => Right(ab.subst[I](x))
        case (x, Right((h, t))) => h match {
          case Op.Map(f) =>
            val newValue = Either.right[F[Any], I[Any]](I.map(x)(f))
            Left(View[I, F, Any, B](newValue, t))
          case Op.Bind(f) =>
            val chunk = f(x).asInstanceOf[View[I, F, Any, Any]]
            Left(View[I, F, Any, B](chunk.value, chunk.ops ++ t))
        }
      }

    override def go(run: F ~> I)(implicit I: Monad[I]): I[B] =
      I.flatten(
        I.tailRecM[FreeRM[I, F, B], I[B]]
          (this: FreeRM[I, F, B])
          (x => I.pure(x.step(run)(I))))
  }

  def pure[I[_], F[_], A](a: I[A]): FreeRM[I, F, A] =
    View[I, F, A, A](Right(a), TASeq.empty)

  def suspend[I[_], F[_], A](a: F[A]): FreeRM[I, F, A] =
    View[I, F, A, A](Left(a), TASeq.empty)

  implicit def relMonad[I[_], F[_]]: RelMonad[I, FreeRM[I, F, ?]] =
    new RelMonad[I, FreeRM[I, F, ?]] {
      override def pure[A](a: I[A]): FreeRM[I, F, A] =
        FreeRM.pure[I, F, A](a)
      override def map[A, B](fa: FreeRM[I, F, A])(f: A => B): FreeRM[I, F, B] =
        fa.map(f)
      override def bind[A, B](fa: FreeRM[I, F, A])(f: I[A] => FreeRM[I, F, B]): FreeRM[I, F, B] =
        fa.bind(f)
    }
}
