package sio.regions

/**
  * A handle to a finalizer that allows you to duplicate it to a parent region using "dup".
  * Duplicating a finalizer means that instead of being performed when the current region
  * terminates, it is performed when the parent region terminates.
  */
final case class FinalizerHandle[R[_]](finalizer: RefCountedFinalizer) extends AnyVal
