package sio.eff

import cats.syntax.all._

import sio.core._
import ops.{Intersection, Union}

import Effect.{ !, !! }

final class EffIO[E <: !!, A] private (val runEff: IO[A]) {
  def map[B](f: A => B): EffIO[E, B] =
    new EffIO(runEff.map(f))
  def flatMap[B, F <: !!, G <: !!](f: A => EffIO[F, B])(implicit u: Union[E, F, G]): EffIO[G, B] =
    new EffIO(runEff.map(f).flatMap(_.runEff))
  def handleErrorWith[F <: !!, G <: !!](f: Throwable => EffIO[F, A])(implicit U: Union[E, F, G]): EffIO[G, A] =
    new EffIO(runEff.handleErrorWith(e => f(e).runEff))
  def into[F <: !!](implicit I: Intersection[E, F, E]): EffIO[F, A] =
    new EffIO(runEff)

  def ensure(error: => Throwable)(predicate: A => Boolean): EffIO[E, A] =
    new EffIO(runEff.flatMap(a => if (predicate(a)) IO.pure(a) else IO.raiseError(error)))

  def attempt: EffIO[E, Either[Throwable, A]] =
    new EffIO(runEff.map(Either.right[Throwable, A]).handleErrorWith(e => IO.pure(Either.left(e))))

  def recover(pf: PartialFunction[Throwable, A]): EffIO[E, A] =
    new EffIO(runEff.handleErrorWith(e => (pf andThen IO.pure) applyOrElse(e, IO.raiseError)))

  def recoverWith[F <: !!, G <: !!](pf: PartialFunction[Throwable, EffIO[F, A]])(implicit U: Union[E, F, G]): EffIO[G, A] =
    new EffIO(runEff.handleErrorWith(e => (pf andThen (x => x.runEff)) applyOrElse(e, IO.raiseError)))

  def >>[F <: !!, G <: !!, B](next: => EffIO[F, B])(implicit u: Union[E, F, G]): EffIO[G, B] =
    new EffIO(runEff.flatMap(a => next.runEff))

  def forever: EffIO[E, Unit] = flatMap[Unit, E, E](_ => forever)(Union.idempotence)
}

object EffIO {
  def lift[E <: EffectSet, A](io: IO[A]): EffIO[E, A] = new EffIO(io)
  def lift1[E <: Effect, A](io: IO[A]): EffIO[Effect.fx1[E], A] = new EffIO(io)
  def apply[E <: EffectSet, A](action: => Impure[A]): EffIO[E, A] = new EffIO(IO(action))
  def pure[E <: EffectSet, A](x: A): EffIO[E, A] = new EffIO(IO.pure(x))
}