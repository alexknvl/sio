package sio.dmz

sealed abstract class Op extends Product with Serializable
object Op {
  final case class Map(f: Val => Val) extends Op
  final case class Bind(f: Val => Thunk) extends Op
  final case class Handle(f: Throwable => Thunk) extends Op
}