package sio.core.syntax

import cats.kernel.Order
import sio.core.{STArray, ST, Mutable}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Sorting

trait STArraySyntax {
  implicit class STArraySyntax[S, E](handle: STArray[S, E]) extends Mutable.Syntax(handle) {
    def length: ST[S, Int] = lift { _.length }

    def get(i: Int): ST[S, E] = lift { _.apply(i) }
    def set(i: Int, x: E): ST[S, Unit] = lift { _.update(i, x) }

    def transform(f: E => E): ST[S, Unit] = lift { a =>
      @tailrec def go(i: Int): Unit =
        if (i < a.length) {
          a.update(i, f(a(i)))
          go(i + 1)
        } else ()
      go(0)
    }

    def stableSort(implicit E: Order[E], CT: ClassTag[E]): ST[S, Unit] =
      lift { a => Sorting.stableSort(a)(CT, E.toOrdering) }
    def quickSort(implicit E: Order[E], CT: ClassTag[E]): ST[S, Unit] =
      lift { a => Sorting.quickSort(a)(E.toOrdering) }
  }
}
