package sio.base.free

import cats.{Id, Monad, MonadError, ~>}
import sio.base.RelMonad
import sio.base.data.{ArrayQueue, Steque, TASeq}
import cats.instances.either._

import scala.annotation.tailrec
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

object `package` {
  trait FreeRMImpl {
    type T[I[_], F[_], +A]

    def unit[I[_], F[_]]: T[I, F, Unit]
    def pure[I[_], F[_], A](a: I[A]): T[I, F, A]
    def suspend[I[_], F[_], A](a: F[A]): T[I, F, A]

    def map[I[_], F[_], A, B](fa: T[I, F, A])(f: A => B): T[I, F, B]
    def bind[I[_], F[_], A, B](fa: T[I, F, A])(f: I[A] => T[I, F, B]): T[I, F, B]

    def step[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): Either[T[I, F, A], I[A]]
    def foldMap[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): I[A]

    def foldEither[E, F[_], A]
    (fa: T[Either[E, ?], F, A], run: F ~> Either[E, ?]): Either[E, A]
  }

  type FreeRM[I[_], F[_], +A] = FreeRM.T[I, F, A]
  final val FreeRM: FreeRMImpl = new FreeRMImpl {
    sealed abstract class Op[I[_], F[_], -A, +B] extends Product with Serializable
    object Op {
      final case class Pure[I[_], F[_], B](a: I[B]) extends Op[I, F, Any, B]
      final case class Suspend[I[_], F[_], B](a: F[B]) extends Op[I, F, Any, B]
      final case class Map[I[_], F[_], A, B](f: A => B) extends Op[I, F, A, B]
      final case class Bind[I[_], F[_], A, B](f: I[A] => T[I, F, B]) extends Op[I, F, A, B]
    }

    type T[I[_], F[_], +A] = TASeq[Op[I, F, ?, ?], Unit, A @uV]

    final def unit[I[_], F[_]]: T[I, F, Unit] =
      TASeq.empty
    final def pure[I[_], F[_], A](a: I[A]): T[I, F, A] =
      TASeq.single[Op[I, F, ?, ?], Unit, A](Op.Pure(a))
    final def suspend[I[_], F[_], A](a: F[A]): T[I, F, A] =
      TASeq.single[Op[I, F, ?, ?], Unit, A](Op.Suspend(a))

    final def map[I[_], F[_], A, B](fa: T[I, F, A])(f: A => B): T[I, F, B] =
      TASeq.append[Op[I, F, ?, ?], Unit, A, B](fa)(Op.Map[I, F, A, B](f))
    final def bind[I[_], F[_], A, B](fa: T[I, F, A])(f: I[A] => T[I, F, B]): T[I, F, B] =
      TASeq.append[Op[I, F, ?, ?], Unit, A, B](fa)(Op.Bind[I, F, A, B](f))

    final def step[I[_], F[_], A]
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

    final def foldMap[I[_], F[_], A]
    (fa: T[I, F, A], run: F ~> I)(implicit I: Monad[I]): I[A] =
      I.flatten(
        I.tailRecM[T[I, F, A], I[A]]
          (fa: T[I, F, A])
          (x => I.pure(step[I, F, A](x, run)(I))))

    @inline final def foldEither[E, F[_], A]
    (fa: T[Either[E, ?], F, A], run: F ~> Either[E, ?]): Either[E, A] = {
      val M: MonadError[Either[E, ?], E] = MonadError[Either[E, ?], E]
      val anyUnit = ().asInstanceOf[Any]
      val rightUnit = Right[E, Any](anyUnit)

      def unwrap[X](t: T[Either[E, ?], F, X]): Steque[Op[Either[E, ?], F, Any, Any]] =
        TASeq.unsafeUnwrap[Op[Either[E, ?], F, ?, ?], Unit, X](t)

      val queue = ArrayQueue.from(unwrap[A](fa).toArray)

      @tailrec def loop(value: Either[E, Any]): Either[E, Any] = {
        val next = queue.popHead()
        next match {
          case None => value
          case Some(Op.Pure(x)) =>

            loop(value.flatMap(_ => x))
          case Some(Op.Suspend(x)) =>
            loop(value.flatMap(_ => run.apply(x)))
          case Some(Op.Map(f)) =>
            loop(value.map(f))
          case Some(Op.Bind(f)) =>
            loop({
              queue.prependAll(unwrap[Any](f(value)).toArray)
              rightUnit
            })
        }
      }

      loop(rightUnit).map(_.asInstanceOf[A])
    }
  }

  implicit def relMonad[I[_], F[_]]: RelMonad[I, FreeRM[I, F, ?]] = new RelMonad[I, FreeRM[I, F, ?]] {
    final def pure[A](a: I[A]): FreeRM[I, F, A] = FreeRM.pure[I, F, A](a)
    final def map[A, B](fa: FreeRM[I, F, A])(f: A => B): FreeRM[I, F, B] = FreeRM.map(fa)(f)
    final def bind[A, B](fa: FreeRM[I, F, A])(f: I[A] => FreeRM[I, F, B]): FreeRM[I, F, B] = FreeRM.bind(fa)(f)
  }

  trait RealIOImpl {
    type T[+A]

    def unit: T[Unit]
    def pure[A](a: A): T[A]
    def raise(e: Throwable): T[Nothing]

    def map[A, B](fa: T[A])(f: A => B): T[B]
    def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B]
    def handle[A](fa: T[A])(f: Throwable => T[A]): T[A]

    def run[A](action: T[A]): Either[Throwable, A]
  }

  type RealIO[+A] = RealIO.T[A]
  val RealIO: RealIOImpl = new RealIOImpl {
    final case class Handle[A](f: Throwable => T[A])
    sealed abstract class List[+A] {
      @inline def ::[AA >: A](a: AA): List[AA] = new ::[AA](a, this)
    }
    final case object Nil extends List[Nothing]
    final case class ::[A](head: A, tail: List[A]) extends List[A]
    final case class Pure[A](head: A, tail: List[A]) extends List[A]
    type T[+A] = List[AnyRef]

    @inline final val unit: T[Unit] =
      Nil
    @inline final def pure[A](a: A): T[A] =
      ((_: Unit) => a) :: Nil
    @inline final def raise(e: Throwable): T[Nothing] =
      ((_: Unit) => throw e) :: Nil

    @inline final def map[A, B](fa: T[A])(f: A => B): T[B] =
      f :: fa
    @inline final def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B] =
      f :: fa
    @inline final def handle[A](fa: T[A])(f: Throwable => T[A]): T[A] =
      Handle(f) :: fa

    @inline final def run[A](action: T[A]): Either[Throwable, A] = {
      val queue = ArrayBuffer.empty[AnyRef]
      @tailrec def prependAll(l: List[AnyRef]): Unit = l match {
        case Nil => ()
        case x :: xs =>
          queue.append(x)
          prependAll(xs)
      }
      prependAll(action)
      var result: Any = ()
      var exc: Throwable = null

      while (queue.nonEmpty) {
        try {
          while (queue.nonEmpty) {
            val last = queue.last
            queue.reduceToSize(queue.length - 1)
            last match {
              case handle: Handle[Any] =>
                if (exc != null) {
                  prependAll(handle.f(exc))
                  result = ()
                  exc = null
                }
              case f =>
                if (exc == null) {
                  f.asInstanceOf[Any => Any](result) match {
                    case s: List[AnyRef] =>
                      prependAll(s)
                      result = ()
                    case r =>
                      result = r
                  }
                }
            }
          }
        } catch {
          case NonFatal(e) =>
            exc = e
            result = null
        }
      }

      if (exc == null) Right(result.asInstanceOf[A])
      else Left(exc)
    }
  }
}
