package sio.base.data

import leibniz.{Is, ===}

final case class TASeq[F[_, _], A, B](seq: Catenable[F[Any, Any]]) { ab =>
  def ++[C](bc: TASeq[F, A, C]): TASeq[F, A, C] =
    TASeq[F, A, C](ab.seq ++ bc.seq)
  def :+[C](bc: F[B, C]): TASeq[F, A, C] =
    TASeq[F, A, C](ab.seq :+ bc.asInstanceOf[F[Any, Any]])
  def +:[C](ca: F[C, A]): TASeq[F, C, B] =
    TASeq[F, C, B](ca.asInstanceOf[F[Any, Any]] +: ab.seq)

  def uncons: Either[A === B, (F[A, Any], TASeq[F, Any, B])] =
    seq.uncons match {
      case None =>
        Left(Is.unsafeForce[A, B])
      case Some((h, t)) =>
        Right((h.asInstanceOf[F[A, Any]], TASeq[F, Any, B](t)))
    }
}
object TASeq {
  def empty[F[_, _], A]: TASeq[F, A, A] = TASeq[F, A, A](Catenable.empty)
  def lift[F[_, _], A, B](ab: F[A, B]): TASeq[F, A, B] =
    TASeq[F, A, B](Catenable.single(ab.asInstanceOf[F[Any, Any]]))
}
