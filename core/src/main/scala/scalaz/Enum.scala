package scalaz

////
/**
 * An [[scalaz.Order]]able with discrete values.
 */
////
trait Enum[F] extends Order[F] { self =>
  ////

  def succ(a: F): F
  def pred(a: F): F

  // derived functions

  def succn(n: Int, a: F): F = Enum.succn(n, a)(self)
  def predn(n: Int, a: F): F = Enum.predn(n, a)(self)

  def min: Option[F] =
    None
  def max: Option[F] =
    None

  /**
   * Moves to the successor, unless at the maximum.
   */
  def succx: Kleisli[Option, F, F] =
    Kleisli(a => if(max forall (equal(a, _))) None else Some(succ(a)))

  /**
   * Moves to the predecessor, unless at the minimum.
   */
  def predx: Kleisli[Option, F, F] =
    Kleisli(a => if(min forall (equal(a, _))) None else Some(pred(a)))

  /**
   * Produce a state value that executes the successor (`succ`) on each spin and executing the given function on the current value. This is useful to implement incremental looping. Evaluating the state value requires a beginning to increment from.
   *
   * @param f The function to execute on each spin of the state value.
   */
  def succState[X](f: F => X): State[F, X] =
    State((s: F) => (succ(s), f(s)))

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and increments through a state value with the given binding function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   * @param m The implementation of the zero function from which to start.
   */
  def succStateZeroM[X, Y](f: F => X, k: X => State[F, Y])(implicit m: Monoid[F]): Y =
    (succState(f) flatMap k) eval m.zero

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and increments through a state value with the given mapping function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   * @param m The implementation of the zero function from which to start.
   */
  def succStateZero[X, Y](f: F => X, k: X => Y)(implicit m: Monoid[F]): Y =
    succStateZeroM(f, (a: X) => State.state[F, Y](k(a)))

  /**
   * Produce a value that starts at the minimum (if it exists) and increments through a state value with the given binding function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   */
  def succStateMinM[X, Y](f: F => X, k: X => State[F, Y]): Option[Y] =
    min map ((succState(f) flatMap k) eval _)

  /**
   * Produce a value that starts at the minimum (if it exists) and increments through a state value with the given mapping function. This is useful to implement incremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   */
  def succStateMin[X, Y](f: F => X, k: X => Y): Option[Y] =
    succStateMinM(f, (a: X) => State.state[F, Y](k(a)))

  /**
   * Produce a state value that executes the predecessor (`pred`) on each spin and executing the given function on the current value. This is useful to implement decremental looping. Evaluating the state value requires a beginning to decrement from.
   *
   * @param f The function to execute on each spin of the state value.
   */
  def predState[X](f: F => X): State[F, X] =
    State((s: F) => (pred(s), f(s)))

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and decrements through a state value with the given binding function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   * @param m The implementation of the zero function from which to start.
   */
  def predStateZeroM[X, Y](f: F => X, k: X => State[F, Y])(implicit m: Monoid[F]): Y =
    (predState(f) flatMap k) eval m.zero

  /**
   * Produce a value that starts at zero (`Monoid.zero`) and decrements through a state value with the given mapping function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   * @param m The implementation of the zero function from which to start.
   */
  def predStateZero[X, Y](f: F => X, k: X => Y)(implicit m: Monoid[F]): Y =
    predStateZeroM(f, (a: X) => State.state[F, Y](k(a)))

  /**
   * Produce a value that starts at the maximum (if it exists) and decrements through a state value with the given binding function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The binding function.
   */
  def predStateMaxM[X, Y](f: F => X, k: X => State[F, Y]): Option[Y] =
    max map ((predState(f) flatMap k) eval _)

  /**
   * Produce a value that starts at the maximum (if it exists) and decrements through a state value with the given mapping function. This is useful to implement decremental looping.
   *
   * @param f The function to execute on each spin of the state value.
   * @param k The mapping function.
   */
  def predStateMax[X, Y](f: F => X, k: X => Y): Option[Y] =
    predStateMaxM(f, (a: X) => State.state[F, Y](k(a)))

  import Free._
  import std.function._

  def from(a: F): EphemeralStream[F] =
    EphemeralStream.cons(a, from(succ(a)))

  def fromStep(n: Int, a: F): EphemeralStream[F] =
    EphemeralStream.cons(a, fromStep(n, succn(n, a)))

  def fromTo(a: F, z: F): EphemeralStream[F] =
    EphemeralStream.cons(a,
      if(equal(a, z))
        EphemeralStream.emptyEphemeralStream
      else
        fromTo(if(lessThan(a, z)) succ(a) else pred(a), z)
    )

  def fromToL(a: F, z: F): List[F] = {
    def fromToLT(a: F, z: F): Trampoline[List[F]] =
      if(equal(a, z))
        return_(a :: Nil)
      else
        suspend(fromToLT(if(lessThan(a, z)) succ(a) else pred(a), z) map (a :: _))
    fromToLT(a, z).run
  }

  def fromStepTo(n: Int, a: F, z: F): EphemeralStream[F] = {
    val cmp = Need {
      if(n > 0)
        greaterThan(_, _)
      else if(n < 0)
        lessThan(_, _)
      else
        (_: F, _: F) => false
    }
    EphemeralStream.cons(a, {
      val k = succn(n, a)
      if (cmp.value(k, z))
        EphemeralStream.emptyEphemeralStream
      else
        fromStepTo(n, k, z)
    })
  }

  def fromStepToL(n: Int, a: F, z: F): List[F] = {
    def fromStepToLT(n: Int, a: F, z: F): Trampoline[List[F]] = {
      val cmp = Need {
        if(n > 0)
          greaterThan(_, _)
        else if(n < 0)
          lessThan(_, _)
        else
          (_: F, _: F) => false
      }
      val k = succn(n, a)
      if (cmp.value(k, z))
        return_(a :: Nil)
      else
        suspend(fromStepToLT(n, k, z) map (a :: _))
    }
    fromStepToLT(n, a, z).run
  }

  trait EnumLaw extends OrderLaw {
    def succpred(x: F): Boolean =
      equal(succ(pred(x)), x)

    def predsucc(x: F): Boolean =
      equal(pred(succ(x)), x)

    def minmaxpred: Boolean =
      min forall (x => max forall (y => equal(pred(x), y)))

    def minmaxsucc: Boolean =
      min forall (x => max forall (y => equal(succ(y), x)))

    def succn(x: F, n: Int): Boolean =
      equal(self.succn(n, x), Enum.succn(n, x)(self))

    def predn(x: F, n: Int): Boolean =
      equal(self.predn(n, x), Enum.predn(n, x)(self))

    def succorder(x: F): Boolean =
      (max exists (equal(_, x))) || greaterThanOrEqual(succ(x), x)

    def predorder(x: F): Boolean =
      (min exists (equal(_, x))) || lessThanOrEqual(pred(x), x)
  }

  def enumLaw = new EnumLaw {}

  ////
  val enumSyntax = new scalaz.syntax.EnumSyntax[F] { def F = Enum.this }
}

object Enum {
  @inline def apply[F](implicit F: Enum[F]): Enum[F] = F

  ////
  def succn[F](n: Int, a: F)(implicit F: Enum[F]): F = {
    var w = n
    var z = a
    while(w < 0) {
      z = F.pred(z)
      w = w + 1
    }
    while(w > 0) {
      z = F.succ(z)
      w = w - 1
    }
    z
  }

  def predn[F](n: Int, a: F)(implicit F: Enum[F]): F = {
    var w = n
    var z = a
    while(w < 0) {
      z = F.succ(z)
      w = w + 1
    }
    while(w > 0) {
      z = F.pred(z)
      w = w - 1
    }
    z
  }
  ////
}
