package sio.core.internal

import cats.data.Xor

import scala.annotation.tailrec

trait Exists[F[_]] {
  type A
  def value: F[A]
}
object Exists {
  def apply[F[_], A0](fa: F[A0]): Exists[F] = new Exists[F] {
    type A = A0
    def value: F[A0] = fa
  }
}

trait Fold[F[_, _], S[_]] {
  def apply[A, B](acc: S[A], next: F[A, B]): S[B]
}
trait FoldUnfold[F[_, _], S[_]] {
  type Result[B] = Exists[λ[X => (S[X], TypeAligned[F, X, B])]]
  final def none[B](value: S[B]): Result[B] =
    Exists[λ[X => (S[X], TypeAligned[F, X, B])], B]((value, TypeAligned.empty[F, B]))
  final def many[Z, B](value: S[Z], seq: TypeAligned[F, Z, B]): Result[B] =
    Exists[λ[X => (S[X], TypeAligned[F, X, B])], Z]((value, seq))

  def apply[A, B](acc: S[A], next: F[A, B]): Result[B]
}
trait Rewrite[F[_, _], S[_]] {
  type Result[B, R] = Exists[λ[X => (S[X], TypeAligned[F, X, B])]] Xor S[R]

  final def none[B, R](value: S[B]): Result[B, R] =
    Xor.left(Exists[λ[X => (S[X], TypeAligned[F, X, B])], B]((value, TypeAligned.empty[F, B])))
  final def many[Z, B, R](value: S[Z], seq: TypeAligned[F, Z, B]): Result[B, R] =
    Xor.left(Exists[λ[X => (S[X], TypeAligned[F, X, B])], Z]((value, seq)))
  final def done[B, R](value: S[R]): Result[B, R] =
    Xor.right(value)

  def apply[A, B, R](acc: S[A], next: F[A, B]): Result[B, R]
}

final case class TypeAligned[F[_, _], A, B](repr: Vector[F[Val, Val]]) {
  def :+[C](op: F[B, C]): TypeAligned[F, A, C] = TypeAligned[F, A, C](repr :+ Val.castK2(op))
  def +:[C](op: F[C, A]): TypeAligned[F, C, B] = TypeAligned[F, C, B](Val.castK2(op) +: repr)
  def ++[C](that: TypeAligned[F, B, C]): TypeAligned[F, A, C] = TypeAligned[F, A, C](repr ++ that.repr)

  def fold[S[_]](s: S[A], f: Fold[F, S]): S[B] = {
    @tailrec def go(state: S[Val], rest: Vector[F[Val, Val]]): S[Val] =
      rest.headOption match {
        case None => state
        case Some(h) => go(f.apply[Val, Val](state, h), rest.tail)
      }

    Val.reifyK[S, B](go(Val.castK(s), repr))
  }

  def foldUnfold[S[_]](s: S[A], f: FoldUnfold[F, S]): S[B] = {
    @tailrec def go(state: S[Val], rest: Vector[F[Val, Val]]): S[Val] =
      rest.headOption match {
        case None => state
        case Some(h) =>
          val result = f.apply[Val, Val](state, h)
          go(Val.castK(result.value._1), result.value._2.repr ++ rest.tail)
      }

    Val.reifyK[S, B](go(Val.castK(s), repr))
  }

  def rewrite[S[_]](s: S[A], f: Rewrite[F, S]): S[B] = {
    @tailrec def go(state: S[Val], rest: Vector[F[Val, Val]]): S[Val] =
      rest.headOption match {
        case None => state
        case Some(h) =>
          f.apply[Val, Val, Val](state, h) match {
            case Xor.Left(x) => go(Val.castK(x.value._1), x.value._2.repr ++ rest.tail)
            case Xor.Right(x) => x
          }
      }

    Val.reifyK[S, B](go(Val.castK(s), repr))
  }
}

object TypeAligned {
  def empty[F[_, _], A]: TypeAligned[F, A, A] =
    new TypeAligned[F, A, A](Vector.empty)
  def one[F[_, _], A, B](fab: F[A, B]): TypeAligned[F, A, B] =
    new TypeAligned[F, A, B](Vector(Val.castK2(fab)))
  def two[F[_, _], A, B, C](fab: F[A, B], fbc: F[B, C]): TypeAligned[F, A, C] =
    new TypeAligned[F, A, C](Vector(Val.castK2(fab), Val.castK2(fbc)))

  def apply[F[_, _], A, B](fab: F[A, B]): TypeAligned[F, A, B] = one(fab)
  def apply[F[_, _], A, B, C](fab: F[A, B], fbc: F[B, C]): TypeAligned[F, A, C] = two(fab, fbc)
}
