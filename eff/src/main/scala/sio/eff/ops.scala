package sio.eff

import shapeless.=:!=

@SuppressWarnings(Array("org.wartremover.warts.Null", "org.wartremover.warts.ExplicitImplicitTypes"))
object ops {
  import EffectSet.{Cons, Nil}

  /**
    * union([], M, M)        :- !.
    * union([H|T], M, [H|O]) :- union(T, M, O), filterNot(M, H, M), !.
    * union([H|T], M, [H|O]) :- remove(M, H, MR), union(T, MR, O), !.
    * union([H|T], M, [H|O]) :- union(T, M, O), !.
    */
  trait Union[L <: EffectSet, M <: EffectSet, Out <: EffectSet]
  object Union extends UnionLowPriority {
    def idempotence[L <: EffectSet]: Union[L, L, L] = null

    implicit def one[M <: EffectSet]: Union[Nil, M, M] = null
    implicit def two[H <: Effect, T <: EffectSet, M <: EffectSet, O <: EffectSet]
    (implicit u: Union[T, M, O], f: FilterNot[M, H, M]): Union[Cons[H, T], M, Cons[H, O]] = null
    implicit def three[H <: Effect, T <: EffectSet, M <: EffectSet, MR <: EffectSet, O <: EffectSet]
    (implicit r: Remove[M, H, MR], u: Union[T, MR, O]): Union[Cons[H, T], M, Cons[H, O]] = null
  }
  trait UnionLowPriority {
    implicit def four[H <: Effect, T <: EffectSet, M <: EffectSet, O <: EffectSet]
    (implicit u: Union[T, M, O]): Union[Cons[H, T], M, Cons[H, O]] = null
  }

  /**
    * remove([H|T], H, T)        :- !.
    * remove([H|T], E, [H|OutT]) :- remove(T, E, OutT), !.
    */
  trait Remove[L <: EffectSet, T <: Effect, Out <: EffectSet]
  object Remove extends RemoveLowPriority {
    implicit def remove[H <: Effect, T <: EffectSet]: Remove[Cons[H, T], H, T] = null
  }
  trait RemoveLowPriority {
    implicit def recurse[H <: Effect, T <: EffectSet, X <: Effect, OutT <: EffectSet]
    (implicit r : Remove[T, X, OutT]): Remove[Cons[H, T], X, Cons[H, OutT]] = null
  }

  /**
    * filterNot(L, U, LSuffix) :- partition(L, U, _, LSuffix).
    */
  trait FilterNot[L <: EffectSet, U <: Effect, Out <: EffectSet]
  object FilterNot {
    implicit def one[L <: EffectSet, U <: Effect, LPrefix <: EffectSet, LSuffix <: EffectSet]
    (implicit partition: Partition[L, U, LPrefix, LSuffix]): FilterNot[L, U, LSuffix] = null
  }

  /**
    * partition([], _, [], [])                  :- !.
    * partition([H|L], H, [H|LPrefix], LSuffix) :- partition(L, H, LPrefix, LSuffix), !.
    * partition([H|L], U, LPrefix, [H|LSuffix]) :- partition(L, U, LPrefix, LSuffix), U \= H, !.
    */
  trait Partition[L <: EffectSet, U, Prefix <: EffectSet, Suffix <: EffectSet]
  object Partition {
    implicit def one[U <: Effect]: Partition[Nil, U, Nil, Nil] = null
    implicit def two[H <: Effect, L <: EffectSet, LPrefix <: EffectSet, LSuffix <: EffectSet]
    (implicit p: Partition[L, H, LPrefix, LSuffix]): Partition[Cons[H, L], H, Cons[H, LPrefix], LSuffix] = null
  }
  trait PartitionLowPriority {
    implicit def three[H <: Effect, L <: EffectSet, U, LPrefix <: EffectSet, LSuffix <: EffectSet]
    (implicit p: Partition[L, U, LPrefix, LSuffix], n: U =:!= H): Partition[Cons[H, L], U, LPrefix, Cons[H, LSuffix]] = null
  }

  /**
    * intersection([], _, [])       :- !.
    * intersection([H|T], M, O)     :- intersection(T, M, O), filterNot(M, H, M).
    * intersection([H|T], M, [H|O]) :- remove(M, H, MR), intersection(T, MR, O).
    */
  trait Intersection[First <: EffectSet, Second <: EffectSet, Intersection <: EffectSet]
  object Intersection {
    implicit def one[L <: EffectSet]: Intersection[Nil, L, Nil] = null
    implicit def two[H <: Effect, T <: EffectSet, M <: EffectSet, O <: EffectSet]
    (implicit i: Intersection[T, M, O], f: FilterNot[M, H, M]): Intersection[Cons[H, T], M, O] = null
    implicit def three[H <: Effect, T <: EffectSet, M <: EffectSet, O <: EffectSet, MR <: EffectSet]
    (implicit r: Remove[M, H, MR], i: Intersection[T, MR, O]): Intersection[Cons[H, T], M, O] = null
  }
}
