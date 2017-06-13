import java.util.concurrent.Callable

import cats.data.EitherT
import leibniz.{===, Forall}
import sio.base.free.`package`.RealIO
import sio.core._
import sio.teletype._
import sio.core.syntax.st._

object core {
  val getUserHome: IO[String] = IO { Option(System.getProperty("user.home")).get }
  def run = for {
    h <- getUserHome
    _ <- putStr("What's your name? ")
    n <- getLine
    _ <- putStrLn(s"Hi, $n, your home directory is $h!")
  } yield ()


//  object FDef {
//    trait Foo {
//      type F[_, +_]
//
//      def pure[S, A](a: A): F[S, A]
//    }
//    final val Foo: Foo = new Foo {
//      type F[S, +A] = A
//      def pure[S, A](a: A): F[S, A] = a
//    }
//    type F[S, +A] = Foo.F[S, A]
//  }
//
//  object syntax {
//    import FDef._
//    implicit class FOps[S, A](val f: F[S, A]) extends AnyVal {
//      def map[B](f: A => B): F[S, B] = ???
//      def blah: F[S, Int] = f.map(_ => 1)
//    }
//  }
//  import FDef._
//  import syntax._
//
//  (Foo.pure[Int, Int](10) : F[Int, Int]).blah.map(_ + 1)

}


object test {

  type ST[S, +A] = ST.T[S, A]

  trait STImpl {
    type T[_, +_]

    def map[S, A, B](fa: T[S, A])(f: A => B): T[S, B] = ???
  }

  final val ST: STImpl = new STImpl {
    type T[S, +A] = RealIO[A]
  }
  final class STSyntaxOps[S, A](val value: ST.T[S, A]) extends AnyVal {
    def map[B](f: A => B): ST[S, B] = ???

    def asCallback(implicit ev: S === RW): ST[RW, () => Impure[A]] = ???

    def asRunnable(implicit ev: S === RW): ST[RW, Runnable] =
      ST.map(asCallback(ev)){ (f: () => A) => new Runnable { override def run(): Unit = f() } }

    def asCallable(implicit ev: S === RW): ST[RW, Callable[A]] =
      ST.map(asCallback(ev)){  (f: () => A) => new Callable[A] { override def call(): A = f() } }
  }


}