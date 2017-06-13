package sio.core

import leibniz.Forall

/** The strict state-transformer monad. A computation of type `ST[S, A]`
  * transforms an internal state indexed by `S`, and returns a value of type `A`.
  */
trait STImpl {
  type T[S, +A]

  def map[S, A, B](fa: T[S, A])(f: A => B): T[S, B]
  def flatMap[S, A, B](fa: T[S, A])(f: A => T[S, B]): T[S, B]
  def handleErrorWith[S, A](fa: T[S, A])(f: Throwable => T[S, A]): T[S, A]

  /** Creates an ST action that produces a unit value without performing
    * any side-effects.
    */
  def unit[S]: T[S, Unit]

  /** Creates an ST action that produces a specific value without performing
    * any side-effects.
    */
  def pure[S, A](x: A): T[S, A]

  /** Creates a failed ST action without any side-effects.
    */
  def raise[S](x: Throwable): T[S, Nothing]

  /** This method allows you to lift arbitrary IO actions into ST monad.
    * For this to be safe, the action should only perform local side-effects.
    */
  def unsafeCapture[S, A](a: => Impure[A]): T[S, A]

  /** This method allows you to convert an ST computation into a callback that
    * can be passed to impure methods. For this to be safe, the impure method
    * taking a callback must not let it escape outside the ST monad.
    */
  def unsafeCallback[S, A, B](action: A => T[S, B]): T[S, A => Impure[B]]

  /** This method allows you to convert an ST computation into a callback that
    * can be passed to impure methods. For this to be safe, the impure method
    * taking a callback must not let it escape outside the ST monad.
    */
  def unsafeCallback0[S, A](action: T[S, A]): T[S, () => Impure[A]]

  /** Prints a message to the standard error output. This function is intended
    * only for debugging and it is neither referentially transparent nor IO-free.
    * It can be useful for investigating bugs or performance problems.
    * Should not be used in production code.
    */
  def trace[S](s: String): T[S, Unit]

  /** Return the value computed by a state transformer computation.
    * The [[ForallST]] ensures that the internal state used by the ST computation
    * is inaccessible to the rest of the program.
    *
    * @see [[unsafeRun]]
    */
  def attempt[A](forallST: Forall[T[?, A]]): Either[Throwable, A]

  /** Return the value computed by a state transformer computation.
    * The [[ForallST]] ensures that the internal state used by the ST computation
    * is inaccessible to the rest of the program.
    *
    * For this call to be safe the computation has to be successful.
    *
    * @return a value of type `A`.
    */
  def unsafeRun[A](forallST: Forall[T[?, A]]): A

  def attemptReal[A](action: T[RW, A]): Either[Throwable, A]

  def unsafeRunReal[A](action: T[RW, A]): A
}