package sio.core.instances

import cats.kernel.Eq
import sio.core.Ref

trait RefInstances {
  implicit def refEq[S, A]: Eq[Ref[S, A]] = Eq.fromUniversalEquals
}
