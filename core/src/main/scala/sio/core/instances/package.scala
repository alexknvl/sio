package sio.core

package object instances {
  object io extends STInstances with IOInstances
  object lift extends LiftIOInstances
  object ref extends RefInstances

  object all extends STInstances with IOInstances with LiftIOInstances with RefInstances
}
