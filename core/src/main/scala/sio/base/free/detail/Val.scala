package sio.base.free.detail

object Val {
  val unit = cast(())

  def cast[A](a: A): Val = a.asInstanceOf[Val]
  def castK1[F[_], A](a: F[A]): F[Val] = a.asInstanceOf[F[Val]]
  def castK2[F[_, _], A, B](f: F[A, B]): F[Val, Val] = f.asInstanceOf[F[Val, Val]]
  def castK21[F[_, _], A, B](f: F[A, B]): F[Val, B] = f.asInstanceOf[F[Val, B]]
  def castK22[F[_, _], A, B](f: F[A, B]): F[A, Val] = f.asInstanceOf[F[A, Val]]

  def reify[A](a: Val): A = a.asInstanceOf[A]
}
