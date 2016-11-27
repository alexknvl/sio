package sio.core

/**
  * Created by alex on 11/27/16.
  */
package object dmz {
  type Val = Any with ({ type Tag = Any })
  type Thunk = Vector[Op]
}
