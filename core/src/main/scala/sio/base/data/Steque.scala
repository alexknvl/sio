/*
Copyright (c) 2013 Paul Chiusano, and respective contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

package sio.base.data

import Steque._

import scala.reflect.ClassTag

/**
  * Trivial catenable sequence. Supports O(1) append, and (amortized)
  * O(1) `uncons`, such that walking the sequence via N successive `uncons`
  * steps takes O(N). Like a difference list, conversion to a `Seq[A]`
  * takes linear time, regardless of how the sequence is built up.
  */
sealed abstract class Steque[+A] {
  /**
    * Returns the head and tail of this catenable if non empty, none otherwise. Amortized O(1).
    */
  final def uncons: Option[(A, Steque[A])] = {
    var c: Steque[A] = this
    val rights = new collection.mutable.ArrayBuffer[Steque[A]]
    var result: Option[(A, Steque[A])] = null
    while (result eq null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            result = None
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
        case Single(a) =>
          val next = if (rights.isEmpty) empty else rights.reduceLeft((x, y) => Append(y,x))
          result = Some(a -> next)
        case Append(l, r) => c = l; rights += r
      }
    }
    result
  }

  /**
    * Returns true if there are no elements in this collection.
    */
  def isEmpty: Boolean

  /**
    * Concatenates this with `c` in O(1) runtime.
    */
  final def ++[B >: A](c: Steque[B]): Steque[B] =
    append(this, c)

  /**
    * Returns a new catenable consisting of `a` followed by this.
    * O(1) runtime.
    */
  final def cons[B >: A](a: B): Steque[B] =
    append(single(a), this)

  /** Alias for [[cons]]. */
  final def +:[B >: A](a: B): Steque[B] =
    cons(a)

  /**
    * Returns a new catenable consisting of this followed by `a`.
    * O(1) runtime.
    */
  final def snoc[B >: A](a: B): Steque[B] =
    append(this, single(a))

  /**
    * Alias for [[snoc]].
    */
  final def :+[B >: A](a: B): Steque[B] =
    snoc(a)

  /**
    * Applies the supplied function to each element and returns a new catenable.
    */
  final def map[B](f: A => B): Steque[B] =
    foldLeft(empty: Steque[B])((acc, a) => acc :+ f(a))

  /**
    * Folds over the elements from left to right using the supplied
    * initial value and function.
    */
  final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var result = z
    foreach(a => result = f(result, a))
    result
  }

  /**
    * Applies the supplied function to each element, left to right.
    */
  final def foreach(f: A => Unit): Unit = {
    var c: Steque[A] = this
    val rights = new collection.mutable.ArrayBuffer[Steque[A]]
    while (c ne null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            c = null
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
        case Single(a) =>
          f(a)
          c = if (rights.isEmpty) Empty else rights.reduceLeft((x, y) => Append(y,x))
          rights.clear()
        case Append(l, r) => c = l; rights += r
      }
    }
  }

  /**
    * Converts to a list.
    */
  final def toList: List[A] = {
    val builder = List.newBuilder[A]
    foreach { a => builder += a; () }
    builder.result
  }

  /**
    * Converts to a list.
    */
  final def toArray[AA >: A](implicit AA: ClassTag[AA]): Array[AA] = {
    val builder = Array.newBuilder[AA]
    foreach { a => builder += a; () }
    builder.result
  }

  override def toString = "Catenable(..)"
}

object Steque {
  private case object Empty extends Steque[Nothing] {
    def isEmpty: Boolean = true
  }
  private final case class Single[A](a: A) extends Steque[A] {
    def isEmpty: Boolean = false
  }
  private final case class Append[A](left: Steque[A], right: Steque[A]) extends Steque[A] {
    def isEmpty: Boolean = false // b/c `append` constructor doesn't allow either branch to be empty
  }

  /** Empty catenable. */
  val empty: Steque[Nothing] = Empty

  /** Creates a catenable of 1 element. */
  def single[A](a: A): Steque[A] = Single(a)

  /** Appends two catenables. */
  def append[A](c: Steque[A], c2: Steque[A]): Steque[A] =
    if (c.isEmpty) c2
    else if (c2.isEmpty) c
    else Append(c, c2)

  /** Creates a catenable from the specified sequence. */
  def fromSeq[A](s: Seq[A]): Steque[A] =
    if (s.isEmpty) empty
    else s.view.reverse.map(single).reduceLeft((x, y) => Append(y, x))

  /** Creates a catenable from the specified elements. */
  def apply[A](as: A*): Steque[A] = {
    as match {
      case w: collection.mutable.WrappedArray[A] =>
        if (w.isEmpty) empty
        else if (w.size == 1) single(w.head)
        else {
          val arr: Array[A] = w.array
          var c: Steque[A] = single(arr.last)
          var idx = arr.length - 2
          while (idx >= 0) {
            c = Append(single(arr(idx)), c)
            idx -= 1
          }
          c
        }
      case _ => fromSeq(as)
    }
  }
}