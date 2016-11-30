package sio

import java.util.{concurrent => C}

import sio.core.{IOHandle, IO}

package object concurrent {
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

  def newEmptyMVar[A]: IO[MVar[A]] = IO {
    new MVar(new C.ArrayBlockingQueue[A](1))
  }

  def newMVar[A](a: A): IO[MVar[A]] = IO {
    val bq = new C.ArrayBlockingQueue[A](1)
    bq.put(a)
    new MVar[A](bq)
  }

  def forkOS(action: IO[Unit]): IO[IOHandle[Thread]] = IO {
    val thread = new Thread(IO.unsafeRunnable(action))
    thread.start()
    IO.handle(thread)
  }

  def forkIO(action: IO[Unit]): IO[Unit] = IO {

  }
}
