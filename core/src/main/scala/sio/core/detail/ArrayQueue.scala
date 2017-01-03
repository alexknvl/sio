package sio.core.detail

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

  private[this] def increaseCapacity(margin: Int = 1): Unit = {
    val newCapacity = math.max(16, (capacity + margin) * 3 / 2)
    val newData = tag.newArray(newCapacity)

    val tailLength =
      if (front > rear) count
      else data.length - rear

    Array.copy(data, rear, newData, 0, tailLength)
    Array.copy(data, 0, newData, tailLength, count - tailLength)

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
      increaseCapacity()
    count += 1
    front = prev(front)
    data(front) = item
  }
  def append(item: T): Unit = {
    if (count == capacity)
      increaseCapacity()
    count += 1
    rear = next(rear)
    data(rear) = item
  }

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
}
