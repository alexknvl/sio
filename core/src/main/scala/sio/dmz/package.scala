package sio

package object dmz {
  type Val = Any with ({ type Tag = Any })
  type Thunk = Vector[Op]
}
