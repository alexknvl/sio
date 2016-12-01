package sio.core

package object instances {
  object io extends IOInstances
  object lift extends LiftIOInstances
  object ref extends RefInstances

  object all extends IOInstances with LiftIOInstances with RefInstances
}
