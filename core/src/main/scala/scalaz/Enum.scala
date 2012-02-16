package scalaz

/*
Laws

1) succ(pred(x)) === x
2) pred(succ(x)) === x
3) min forall (n => max forall (x => pred(n) === x))
4) min forall (n => max forall (x => succ(x) === n))
5) succn(1)(x) === succ(x)
6) predn(1)(x) === pred(x)
7) order(succ(x), x) != LT
8) order(pred(x), x) != GT
*/
trait Enum[A] extends Order[A] {
  def succ: A => A
  def pred: A => A
  def succn: Int => A => A =
    n => a => {
      var w = n
      var z = a
      while(w < 0) {
        z = pred(a)
        w = w + 1
      }
      while(w > 0) {
        z = succ(a)
        w = w - 1
      }
      z
    }
  def predn: Int => A => A =
    n => a => {
      var w = n
      var z = a
      while(w < 0) {
        z = succ(a)
        w = w + 1
      }
      while(w > 0) {
        z = pred(a)
        w = w - 1
      }
      z
    }
  def min: Option[A] =
    None
  def max: Option[A] =
    None

  /**
   * Moves to the successor, unless at the maximum.
   *
   * @param e The equality test for determining if at the maximum.
   */
  def succx(implicit e: Equal[A]): Kleisli[Option, A, A] =
    Kleisli(a => if(max forall (e.equal(a, _))) None else Some(succ(a)))

  /**
   * Moves to the predecessor, unless at the minimum.
   *
   * @param e The equality test for determining if at the minimum.
   */
  def predx(implicit e: Equal[A]): Kleisli[Option, A, A] =
    Kleisli(a => if(min forall (e.equal(a, _))) None else Some(pred(a)))

  /**
   * Produce a state value that executes the successor (`succ`) on each spin and executing the given function on the current value. This is useful to implement incremental looping. Evaluating the state value requires a beginning to increment from.
   *
   * @param f The function to execute on each spin of the state value.
   */
  def succState[X](f: A => X): State[A, X] =
    State((s: A) => (f(s), succ(s)))

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and increments through a state value with the given binding function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   * @param m The implementation of the zero function from which to start.
   */
  def succStateZeroM[X, Y](f: A => X, k: X => State[A, Y])(implicit m: Monoid[A]): Y =
    (succState(f) flatMap k) eval m.zero

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and increments through a state value with the given mapping function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   * @param m The implementation of the zero function from which to start.
   */
  def succStateZero[X, Y](f: A => X, k: X => Y)(implicit m: Monoid[A]): Y =
    succStateZeroM(f, (a: X) => State.state[A, Y](k(a)))

  /**
   * Produce a value that starts at the minimum (if it exists) and increments through a state value with the given binding function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   */
  def succStateMinM[X, Y](f: A => X, k: X => State[A, Y]): Option[Y] =
    min map ((succState(f) flatMap k) eval _)

  /**
   * Produce a value that starts at the minimum (if it exists) and increments through a state value with the given mapping function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   */
  def succStateMin[X, Y](f: A => X, k: X => Y): Option[Y] =
    succStateMinM(f, (a: X) => State.state[A, Y](k(a)))

  /**
   * Produce a state value that executes the predecessor (`pred`) on each spin and executing the given function on the current value. This is useful to implement decremental looping. Evaluating the state value requires a beginning to decrement from.
   *
   * @param f The function to execute on each spin of the state value.
   */
  def predState[X](f: A => X): State[A, X] =
    State((s: A) => (f(s), pred(s)))

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and decrements through a state value with the given binding function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   * @param m The implementation of the zero function from which to start.
   */
  def predStateZeroM[X, Y](f: A => X, k: X => State[A, Y])(implicit m: Monoid[A]): Y =
    (predState(f) flatMap k) eval m.zero

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and decrements through a state value with the given mapping function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   * @param m The implementation of the zero function from which to start.
   */
  def predStateZero[X, Y](f: A => X, k: X => Y)(implicit m: Monoid[A]): Y =
    predStateZeroM(f, (a: X) => State.state[A, Y](k(a)))

  /**
   * Produce a value that starts at the maximum (if it exists) and decrements through a state value with the given binding function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   */
  def predStateMaxM[X, Y](f: A => X, k: X => State[A, Y]): Option[Y] =
    max map ((predState(f) flatMap k) eval _)

  /**
   * Produce a value that starts at the maximum (if it exists) and decrements through a state value with the given mapping function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   */
  def predStateMax[X, Y](f: A => X, k: X => Y): Option[Y] =
    predStateMaxM(f, (a: X) => State.state[A, Y](k(a)))

}
