package sio.concurrent

import sio.core.IO

import java.util.{concurrent => C}

final class MVar[A](unsafeQueue: C.BlockingQueue[A]) {
  /**
    * Put a new value in an [[MVar]], block if empty.
    *
    * @param a
    * @return
    */
  def put(a: A): IO[Unit] = IO { unsafeQueue.put(a) }

  /**
    * Take a value from an [[MVar]], block if empty.
    *
    * @return
    */
  def take: IO[A] = IO { unsafeQueue.take() }

  /**
    * Put a value into an [[MVar]], return false if already full.
    *
    * @param a
    * @return
    */
  def offer(a: A): IO[Boolean] = IO { unsafeQueue.offer(a) }

  /**
    * Get the value from an [[MVar]], return None when empty.
    *
    * @return
    */
  def poll: IO[Option[A]] = IO { Option(unsafeQueue.poll()) }
}

object MVar {
  def empty[A]: IO[MVar[A]] = IO {
    new MVar(new C.ArrayBlockingQueue[A](1))
  }

  def create[A](a: A): IO[MVar[A]] = IO {
    val bq = new C.ArrayBlockingQueue[A](1)
    bq.put(a)
    new MVar[A](bq)
  }
}