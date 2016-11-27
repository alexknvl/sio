package sio.core.dmz

import cats.syntax.either._

object Val {
  val unit = cast(())
  val xorUnit = Either.right[Throwable, Val](Val.unit)

  def cast[A](a: A): Val = a.asInstanceOf[Val]
  def castK2[F[_, _], A, B](a: F[A, B]): F[Val, Val] = a.asInstanceOf[F[Val, Val]]
  def castK21[F[_, _], A, B](a: F[A, B]): F[Val, B] = a.asInstanceOf[F[Val, B]]
  def castK22[F[_, _], A, B](a: F[A, B]): F[A, Val] = a.asInstanceOf[F[A, Val]]

  def reify[A](a: Val): A = a.asInstanceOf[A]
}