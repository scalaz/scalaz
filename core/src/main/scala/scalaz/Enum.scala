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
}

object Enum extends EnumFunctions

trait EnumFunctions {

  /**
   * Produce a state value that executes the successor (`succ`) on each spin and executing the given function on the current value. This is useful to implement incremental looping. Evaluating the state value requires a beginning to increment from.
   *
   * @param f The function to execute on each spin of the state value.
   * @param e The implementation of the successor function.
   */
  def succState[S, A](f: S => A)(implicit e: Enum[S]): State[S, A] =
    State((s: S) => (f(s), e succ s))

  /**
   * Produce a value that starts at a zero (`Monoid.zero`) and increments through a state value with the given binding function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   * @param e The implementation of the successor function.
   * @param m The implementation of the zero function from which to start.
   */
  def succStateZeroM[S, A, B](f: S => A, k: A => State[S, B])(implicit e: Enum[S], m: Monoid[S]): B =
    (succState(f) flatMap k).eval(m.zero)

  /**
   * Produce a value that starts at a zero (`Monoid.zero`) and increments through a state value with the given mapping function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   * @param e The implementation of the successor function.
   * @param m The implementation of the zero function from which to start.
   */
  def succStateZero[S, A, B](f: S => A, k: A => B)(implicit e: Enum[S], m: Monoid[S]): B =
    succStateZeroM(f, (a: A) => State.state[S, B](k(a)))
}