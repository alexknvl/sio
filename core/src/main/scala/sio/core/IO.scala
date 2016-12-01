package sio.core

object IO {
  val unit: IO[Unit] = ST.unit
  def pure[A](x: A): IO[A] = ST.pure(x)
  def raiseError[A](e: Throwable): IO[A] = ST.raiseError(e)
  def capture[A](f: => A): IO[A] = ST.unsafeCapture(f)

  def apply[A](f: => A): IO[A] = capture(f)

  def mutable[A](a: A): IOMutable[A] = new Mutable(a)
  def trace(s: String): IO[Unit] = IO { System.err.println(s) }
  def forkOS(action: IO[Unit]): IO[IOMutable[Thread]] = new ST(Thunk.suspend(Op.ForkOS(action)))
}