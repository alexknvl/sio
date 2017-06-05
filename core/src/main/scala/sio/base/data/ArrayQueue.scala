package sio.base.data

import scala.reflect.ClassTag

final class ArrayQueue[T]
(private val tag: ClassTag[T],
 private var data: Array[T],
 private var front: Int,
 private var rear: Int,
 private var count: Int)
{
  @inline private[this] def capacity: Int = data.length
  @inline private[this] def next(i: Int) = (i + 1) % capacity
  @inline private[this] def prev(i: Int) = (i - 1 + capacity) % capacity

  def ensureCapacity(atLeast: Int): Unit = {
    val newCapacity = math.max(16, math.max(capacity, atLeast * 3 / 2))
    val newData = tag.newArray(newCapacity)

    val tail = math.min(count, data.length - front)
    System.arraycopy(data, front, newData, 0, tail)
    System.arraycopy(data, 0, newData, tail, count - tail)

    data = newData
    front = 0
    rear = count - 1
  }

  def size: Int = count
  def isEmpty: Boolean = count == 0
  def nonEmpty: Boolean = count > 0

  def head: Option[T] = if (count > 0) Some(data(front)) else None
  def last: Option[T] = if (count > 0) Some(data(rear)) else None

  def prepend(item: T): Unit = {
    if (count == capacity)
      ensureCapacity(count + 1)
    count += 1
    front = prev(front)
    data(front) = item
  }

  def prependAll(items: Array[T]): Unit = {
    if (count + items.length >= capacity)
      ensureCapacity(count + items.length)

    count += items.length
    items.reverseIterator.foreach { item =>
      front = prev(front)
      data(front) = item
    }
  }

  def append(item: T): Unit = {
    if (count == capacity)
      ensureCapacity(count + 1)
    count += 1
    rear = next(rear)
    data(rear) = item
  }

  def popHeadOrThrow(): T =
    if (count > 0) {
      val result = data(front)
      front = next(front)
      count -= 1
      result
    } else throw new IllegalStateException("queue is empty")

  def popHead(): Option[T] =
    if (count > 0) {
      val result = data(front)
      front = next(front)
      count -= 1
      Some(result)
    } else None

  def popLast(): Option[T] =
    if (count > 0) {
      val result = data(rear)
      rear = prev(rear)
      count -= 1
      Some(result)
    } else None
}

object ArrayQueue {
  def empty[T](implicit T: ClassTag[T]): ArrayQueue[T] = ofCapacity(16)

  def ofCapacity[T](capacity: Int)(implicit T: ClassTag[T]): ArrayQueue[T] =
    new ArrayQueue[T](
      tag = T,
      data = T.newArray(math.max(0, capacity)),
      front = 0,
      rear = capacity - 1,
      count = 0
    )

  def from[T](arr: Array[T])(implicit T: ClassTag[T]): ArrayQueue[T] =
    new ArrayQueue[T](
      tag = T,
      data = arr,
      front = 0,
      rear = 0,
      count = arr.length
    )
}