package sio.core.internal


object `package` {
  type Val = Any with ({ type Tag = Any })
}

object Val {
  val unit: Val = cast(())

  @inline def cast[A](x: A): Val = x.asInstanceOf[Val]
  @inline def castK[F[_], A](x: F[A]): F[Val] = x.asInstanceOf[F[Val]]
  @inline def castK2[F[_, _], A, B](op: F[A, B]): F[Val, Val] = op.asInstanceOf[F[Val, Val]]

  @inline def reify[A](x: Val): A = x.asInstanceOf[A]
  @inline def reifyK[F[_], A](x: F[Val]): F[A] = x.asInstanceOf[F[A]]
  @inline def reifyK2[F[_, _], A, B](x: F[Val, Val]): F[A, B] = x.asInstanceOf[F[A, B]]
}
