package sio.base.free.detail

import scala.annotation.tailrec
import scala.reflect.ClassTag

final class ArrayQueue[T]
(private var data: Array[T],
 private var front: Int = 0,
 private var rear: Int = -1,
 private var count: Int = 0)
(implicit T: ClassTag[T]) {
  @inline private def capacity: Int = data.length
  @inline private def next(i: Int) =
    (i + 1) % capacity
  @inline private def prev(i: Int) =
    (i - 1 + capacity) % capacity

  private def doubleCapacity(): Unit = {
    val newData = Array.ofDim[T](2 * capacity)
    @tailrec def go(i: Int, j: Int): Unit = {
      newData.update(i, data(j))
      go(i + 1, (j + 1) % capacity)
    }
    go(0, front)

    data = newData
    front = 0
    rear = count - 1
  }

  def head: Option[T] =
    if (count == 0) None
    else Some(data(front))
  def last: Option[T] =
    if (count == 0) None
    else Some(data(rear))

  def prepend(item: T): Unit = {
    if (count == capacity)
      doubleCapacity()

    count += 1
    front = prev(front)
    data(front) = item
  }
  def append(item: T): Unit = {
    if (count == capacity)
      doubleCapacity()

    count += 1
    rear = next(rear)
    data(rear) = item
  }

  def prependAll(items: IndexedSeq[T]): Unit = {
    @tailrec def go(i: Int): Unit =
      if (i >= 0) {
        prepend(items(i))
        go(i - 1)
      }
    go(items.size - 1)
  }
  def appendAll(items: IndexedSeq[T]): Unit = {
    @tailrec def go(i: Int): Unit =
      if (i < items.size) {
        append(items(i))
        go(i + 1)
      }
    go(0)
  }

  def popFront(): Option[T] =
    if (count == 0) None
    else {
      val result = data(front)
      front = next(front)
      count -= 1
      Some(result)
    }
  def popBack(): Option[T] =
    if (count == 0) None
    else {
      val result = data(rear)
      rear = prev(rear)
      count -= 1
      Some(result)
    }

  def size: Int = count
  def isEmpty: Boolean = count == 0
  def nonEmpty: Boolean = count > 0
}

object ArrayQueue {
  def fromArray[T](array: Array[T])(implicit T: ClassTag[T]): ArrayQueue[T] =
    new ArrayQueue[T](array, 0, array.length - 1, array.length)(T)
}
