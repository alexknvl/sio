package sio.eff

trait Effect
object Effect {
  type ! = Effect
  type !! = EffectSet
  type +:[A <: !, B <: !!] = EffectSet.Cons[A, B]

  type fx0                                                       = EffectSet.Nil
  type fx1[E1 <: !]                                              = E1 +: fx0
  type fx2[E1 <: !, E2 <: !]                                     = E1 +: E2 +: fx0
  type fx3[E1 <: !, E2 <: !, E3 <: !]                            = E1 +: E2 +: E3 +: fx0
  type fx4[E1 <: !, E2 <: !, E3 <: !, E4 <: !]                   = E1 +: E2 +: E3 +: E4 +: fx0
  type fx5[E1 <: !, E2 <: !, E3 <: !, E4 <: !, E5 <: !]          = E1 +: E2 +: E3 +: E4 +: E5 +: fx0
  type fx6[E1 <: !, E2 <: !, E3 <: !, E4 <: !, E5 <: !, E6 <: !] = E1 +: E2 +: E3 +: E4 +: E5 +: E6 +: fx0
}

sealed trait EffectSet
object EffectSet {
  sealed trait Nil extends EffectSet
  sealed trait Cons[H <: Effect, T <: EffectSet] extends EffectSet
}