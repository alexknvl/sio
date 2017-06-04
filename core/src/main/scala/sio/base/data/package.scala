package sio.base.data

import leibniz.{Is, ===}

object `package` {
  trait TASeqImpl {
    type T[F[_, _], A, B]

    def empty[F[_, _], A]: T[F, A, A]
    def single[F[_, _], A, B](ab: F[A, B]): T[F, A, B]

    def concat[F[_, _], A, B, C](ab: T[F, A, B])(bc: T[F, B, C]): T[F, A, C]
    def append[F[_, _], A, B, C](ab: T[F, A, B])(bc: F[B, C]): T[F, A, C]
    def prepend[F[_, _], A, B, Z](bc: T[F, A, B])(za: F[Z, A]): T[F, Z, B]

    def uncons[F[_, _], A, B](ab: T[F, A, B]): Either[A === B, (F[A, Any], T[F, Any, B])]
  }

  type TASeq[F[_, _], A, B] = TASeq.T[F, A, B]
  val TASeq: TASeqImpl = new TASeqImpl {
    type T[F[_, _], A, B] = Catenable[F[Any, Any]]

    def empty[F[_, _], A]: T[F, A, A] =
      Catenable.empty
    def single[F[_, _], A, B](ab: F[A, B]): T[F, A, B] =
      Catenable.single(ab.asInstanceOf[F[Any, Any]])

    def concat[F[_, _], A, B, C](ab: T[F, A, B])(bc: T[F, A, C]): T[F, A, C] =
      ab ++ bc
    def append[F[_, _], A, B, C](ab: T[F, A, B])(bc: F[B, C]): T[F, A, C] =
      ab :+ bc.asInstanceOf[F[Any, Any]]
    def prepend[F[_, _], A, B, Z](bc: T[F, A, B])(za: F[Z, A]): T[F, Z, B] =
      za.asInstanceOf[F[Any, Any]] +: bc

    def uncons[F[_, _], A, B](ab: T[F, A, B]): Either[A === B, (F[A, Any], T[F, Any, B])] =
      ab.uncons match {
        case None =>
          Left(Is.unsafeForce[A, B])
        case Some((h, t)) =>
          Right((h.asInstanceOf[F[A, Any]], t))
      }
  }
}
