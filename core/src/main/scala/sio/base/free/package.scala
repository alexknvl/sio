package sio.base.free

import cats.{Id, Monad, ~>}
import sio.base.RelMonad
import sio.base.data.TASeq

object `package` {
  trait FreeRMImpl {
    type T[I[_], F[_], A]

    def unit[I[_], F[_]]: T[I, F, Unit]
    def pure[I[_], F[_], A](a: I[A]): T[I, F, A]
    def suspend[I[_], F[_], A](a: F[A]): T[I, F, A]

    def map[I[_], F[_], A, B](fa: T[I, F, A])(f: A => B): T[I, F, B]
    def bind[I[_], F[_], A, B](fa: T[I, F, A])(f: I[A] => T[I, F, B]): T[I, F, B]

    def step[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): Either[T[I, F, A], I[A]]
    def foldMap[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): I[A]
  }

  type FreeRM[I[_], F[_], A] = FreeRM.T[I, F, A]
  final val FreeRM: FreeRMImpl = new FreeRMImpl {
    sealed abstract class Op[I[_], F[_], -A, +B] extends Product with Serializable
    object Op {
      final case class Pure[I[_], F[_], B](a: I[B]) extends Op[I, F, Any, B]
      final case class Suspend[I[_], F[_], B](a: F[B]) extends Op[I, F, Any, B]
      final case class Map[I[_], F[_], A, B](f: A => B) extends Op[I, F, A, B]
      final case class Bind[I[_], F[_], A, B](f: I[A] => T[I, F, B]) extends Op[I, F, A, B]
    }

    type T[I[_], F[_], A] = TASeq[Op[I, F, ?, ?], Unit, A]

    def unit[I[_], F[_]]: T[I, F, Unit] =
      TASeq.empty
    def pure[I[_], F[_], A](a: I[A]): T[I, F, A] =
      TASeq.single[Op[I, F, ?, ?], Unit, A](Op.Pure(a))
    def suspend[I[_], F[_], A](a: F[A]): T[I, F, A] =
      TASeq.single[Op[I, F, ?, ?], Unit, A](Op.Suspend(a))

    def map[I[_], F[_], A, B](fa: T[I, F, A])(f: A => B): T[I, F, B] =
      TASeq.append[Op[I, F, ?, ?], Unit, A, B](fa)(Op.Map[I, F, A, B](f))
    def bind[I[_], F[_], A, B](fa: T[I, F, A])(f: I[A] => T[I, F, B]): T[I, F, B] =
      TASeq.append[Op[I, F, ?, ?], Unit, A, B](fa)(Op.Bind[I, F, A, B](f))

    def step[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): Either[T[I, F, A], I[A]] =
      TASeq.uncons[Op[I, F, ?, ?], Unit, A](fa) match {
        case Left(proof) =>
          Right(proof.subst[I](I.pure(())))
        case Right((h, t)) => h match {
          case Op.Pure(x) =>
            TASeq.uncons[Op[I, F, ?, ?], Any, A](t) match {
              case Left(proof) =>
                Right(proof.subst[I](x))
              case Right((h2, t2)) =>
                h2 match {
                  case Op.Pure(y) =>
                    Left(TASeq.prepend(t2)(Op.Pure(y)))
                  case Op.Suspend(y) =>
                    Left(TASeq.prepend(t2)(Op.Pure(run.apply(y))))
                  case Op.Map(f) =>
                    Left(TASeq.prepend(t2)(Op.Pure(I.map(x)(f))))
                  case Op.Bind(f) =>
                    val chunk: T[I, F, Any] = f(x)
                    Left(TASeq.concat[Op[I, F, ?, ?], Unit, Any, A](chunk)(t2))
                }
            }

          case Op.Suspend(x) =>
            Left(TASeq.prepend(t)(Op.Pure(run.apply(x))))
          case Op.Map(f) =>
            Left(TASeq.prepend(t)(Op.Pure(I.pure(f(())))))
          case Op.Bind(f) =>
            val chunk: T[I, F, Any] = f(I.pure(()))
            Left(TASeq.concat[Op[I, F, ?, ?], Unit, Any, A](chunk)(t))
        }
      }

    def foldMap[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): I[A] =
      I.flatten(
        I.tailRecM[T[I, F, A], I[A]]
          (fa: T[I, F, A])
          (x => I.pure(step[I, F, A](x, run)(I))))
  }

  implicit def relMonad[I[_], F[_]]: RelMonad[I, FreeRM[I, F, ?]] = new RelMonad[I, FreeRM[I, F, ?]] {
    def pure[A](a: I[A]): FreeRM[I, F, A] = FreeRM.pure[I, F, A](a)
    def map[A, B](fa: FreeRM[I, F, A])(f: A => B): FreeRM[I, F, B] = FreeRM.map(fa)(f)
    def bind[A, B](fa: FreeRM[I, F, A])(f: I[A] => FreeRM[I, F, B]): FreeRM[I, F, B] = FreeRM.bind(fa)(f)
  }
}
