package scalaz
package std

import scalaz._
import Id._

trait AnyValInstances {

  implicit val unitInstance: Monoid[Unit] with Enum[Unit] with Show[Unit] = new Monoid[Unit] with Enum[Unit] with Show[Unit] {
    override def shows(f: Unit) = ().toString

    def append(f1: Unit, f2: => Unit) = ()

    def zero = ()

    def order(x: Unit, y: Unit) = Ordering.EQ

    def succ(u: Unit) = ()

    def pred(u: Unit) = ()

    override def succn(a: Int, b: Unit) = ()

    override def predn(a: Int, b: Unit) = ()

    override def min = Some(())

    override def max = Some(())

    override def equalIsNatural: Boolean = true
  }

  implicit object booleanInstance extends Enum[Boolean] with Show[Boolean] {
    override def shows(f: Boolean) = f.toString

    def order(x: Boolean, y: Boolean) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: Boolean) = !b

    def pred(b: Boolean) = !b

    override def succn(n: Int, b: Boolean) = if(n % 2 == 0) b else !b

    override def predn(n: Int, b: Boolean) = if(n % 2 == 0) b else !b

    override def min = Some(false)

    override def max = Some(true)

    override def equalIsNatural: Boolean = true

    object conjunction extends Monoid[Boolean] {
      def append(f1: Boolean, f2: => Boolean) = f1 && f2

      def zero: Boolean = true
    }

    object disjunction extends Monoid[Boolean] {
      def append(f1: Boolean, f2: => Boolean) = f1 || f2

      def zero = false
    }

  }

  import Tags.{Conjunction, Disjunction}

  implicit val booleanDisjunctionNewTypeInstance: Monoid[Boolean @@ Disjunction] with Enum[Boolean @@ Disjunction] = new Monoid[Boolean @@ Disjunction] with Enum[Boolean @@ Disjunction] {
    def append(f1: Boolean @@ Disjunction, f2: => Boolean @@ Disjunction) = Disjunction(f1 || f2)

    def zero: Boolean @@ Disjunction = Disjunction(false)

    def order(a1: Boolean @@ Disjunction, a2: Boolean @@ Disjunction) = Order[Boolean].order(a1, a2)

    def succ(b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].succ(b))

    def pred(b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].pred(b))

    override def succn(n: Int, b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].succn(n, b))

    override def predn(n: Int, b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].predn(n, b))

    override def min = Enum[Boolean].min map (Disjunction(_))

    override def max = Enum[Boolean].max map (Disjunction(_))

  }

  implicit val booleanConjunctionNewTypeInstance: Monoid[Boolean @@ Conjunction] with Enum[Boolean @@ Conjunction] = new Monoid[Boolean @@ Conjunction] with Enum[Boolean @@ Conjunction] {
    def append(f1: Boolean @@ Conjunction, f2: => Boolean @@ Conjunction) = Conjunction(f1 && f2)

    def zero: Boolean @@ Conjunction = Conjunction(true)

    def order(a1: Boolean @@ Conjunction, a2: Boolean @@ Conjunction) = Order[Boolean].order(a1, a2)

    def succ(b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].succ(b))

    def pred(b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].pred(b))

    override def succn(n: Int, b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].succn(n, b))

    override def predn(n: Int, b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].predn(n, b))

    override def min = Enum[Boolean].min map (Conjunction(_))

    override def max = Enum[Boolean].max map (Conjunction(_))

  }

  implicit val byteInstance: Monoid[Byte] with Enum[Byte] with Show[Byte] = new Monoid[Byte] with Enum[Byte] with Show[Byte] {
    override def shows(f: Byte) = f.toString

    def append(f1: Byte, f2: => Byte) = (f1 + f2).toByte

    def zero: Byte = 0

    def order(x: Byte, y: Byte) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: Byte) = (b + 1).toByte
    def pred(b: Byte) = (b - 1).toByte
    override def succn(a: Int, b: Byte) = (b + a).toByte
    override def predn(a: Int, b: Byte) = (b - a).toByte
    override def min = Some(Byte.MinValue)
    override def max = Some(Byte.MaxValue)

    override def equalIsNatural: Boolean = true

    object multiplication extends Monoid[Byte] {
      def append(f1: Byte, f2: => Byte) = (f1 * f2).toByte

      def zero: Byte = 1
    }

  }

  import Tags.{Multiplication}

  implicit val byteMultiplicationNewType: Monoid[Byte @@ Multiplication] with Enum[Byte @@ Multiplication] = new Monoid[Byte @@ Multiplication] with Enum[Byte @@ Multiplication] {
    def append(f1: Byte @@ Multiplication, f2: => Byte @@ Multiplication) = Multiplication((f1 * f2).toByte)

    def zero: Byte @@ Multiplication = Multiplication(1)

    def order(a1: Byte @@ Multiplication, a2: Byte @@ Multiplication) = Order[Byte].order(a1, a2)

    def succ(b: Byte @@ Multiplication) = Multiplication(Enum[Byte].succ(b))

    def pred(b: Byte @@ Multiplication) = Multiplication(Enum[Byte].pred(b))

    override def succn(n: Int, b: Byte @@ Multiplication) = Multiplication(Enum[Byte].succn(n, b))

    override def predn(n: Int, b: Byte @@ Multiplication) = Multiplication(Enum[Byte].predn(n, b))

    override def min = Enum[Byte].min map (Multiplication(_))

    override def max = Enum[Byte].max map (Multiplication(_))

    override def equalIsNatural: Boolean = true

  }

  implicit val char: Monoid[Char] with Enum[Char] with Show[Char] = new Monoid[Char] with Enum[Char] with Show[Char] {
    override def shows(f: Char) = f.toString

    def append(f1: Char, f2: => Char) = (f1 + f2).toChar

    def zero: Char = 0

    def order(x: Char, y: Char) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: Char) = (b + 1).toChar
    def pred(b: Char) = (b - 1).toChar
    override def succn(a: Int, b: Char) = (b + a).toChar
    override def predn(a: Int, b: Char) = (b - a).toChar
    override def min = Some(Char.MinValue)
    override def max = Some(Char.MaxValue)

    override def equalIsNatural: Boolean = true

    object multiplication extends Monoid[Char] {
      def append(f1: Char, f2: => Char) = (f1 * f2).toChar

      def zero: Char = 1
    }

  }

  implicit val charMultiplicationNewType: Monoid[Char @@ Multiplication] with Enum[Char @@ Multiplication] = new Monoid[Char @@ Multiplication] with Enum[Char @@ Multiplication] {
    def append(f1: Char @@ Multiplication, f2: => Char @@ Multiplication) = Multiplication((f1 * f2).toChar)

    def zero: Char @@ Multiplication = Multiplication(1)

    def order(a1: Char @@ Multiplication, a2: Char @@ Multiplication) = Order[Char].order(a1, a2)

    def succ(b: Char @@ Multiplication) = Multiplication(Enum[Char].succ(b))

    def pred(b: Char @@ Multiplication) = Multiplication(Enum[Char].pred(b))

    override def succn(n: Int, b: Char @@ Multiplication) = Multiplication(Enum[Char].succn(n, b))

    override def predn(n: Int, b: Char @@ Multiplication) = Multiplication(Enum[Char].predn(n, b))

    override def min = Enum[Char].min map (Multiplication(_))

    override def max = Enum[Char].max map (Multiplication(_))

    override def equalIsNatural: Boolean = true
  }

  implicit val shortInstance: Monoid[Short] with Enum[Short] with Show[Short] = new Monoid[Short] with Enum[Short] with Show[Short] {
    override def shows(f: Short) = f.toString

    def append(f1: Short, f2: => Short) = (f1 + f2).toShort

    def zero: Short = 0

    def order(x: Short, y: Short) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: Short) = (b + 1).toShort
    def pred(b: Short) = (b - 1).toShort
    override def succn(a: Int, b: Short) = (b + a).toShort
    override def predn(a: Int, b: Short) = (b - a).toShort
    override def min = Some(Short.MinValue)
    override def max = Some(Short.MaxValue)

    override def equalIsNatural: Boolean = true

    object multiplication extends Monoid[Short] {
      def append(f1: Short, f2: => Short) = (f1 * f2).toShort

      def zero: Short = 1
    }

  }

  implicit val shortMultiplicationNewType: Monoid[Short @@ Multiplication] with Enum[Short @@ Multiplication] = new Monoid[Short @@ Multiplication] with Enum[Short @@ Multiplication] {
    def append(f1: Short @@ Multiplication, f2: => Short @@ Multiplication) = Multiplication((f1 * f2).toShort)

    def zero: Short @@ Multiplication = Multiplication(1)

    def succ(b: Short @@ Multiplication) = Multiplication(Enum[Short].succ(b))

    def pred(b: Short @@ Multiplication) = Multiplication(Enum[Short].pred(b))

    override def succn(n: Int, b: Short @@ Multiplication) = Multiplication(Enum[Short].succn(n, b))

    override def predn(n: Int, b: Short @@ Multiplication) = Multiplication(Enum[Short].predn(n, b))

    override def min = Enum[Short].min map (Multiplication(_))

    override def max = Enum[Short].max map (Multiplication(_))

    def order(a1: Short @@ Multiplication, a2: Short @@ Multiplication) = Order[Short].order(a1, a2)
  }

  implicit val intInstance: Monoid[Int] with Enum[Int] with Show[Int] = new Monoid[Int] with Enum[Int] with Show[Int] {
    override def shows(f: Int) = f.toString

    def append(f1: Int, f2: => Int) = f1 + f2

    def zero: Int = 0

    def order(x: Int, y: Int) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: Int) = b + 1
    def pred(b: Int) = b - 1
    override def succn(a: Int, b: Int) = b + a
    override def predn(a: Int, b: Int) = b - a
    override def min = Some(Int.MinValue)
    override def max = Some(Int.MaxValue)

    override def equalIsNatural: Boolean = true

    object multiplication extends Monoid[Int] {
      def append(f1: Int, f2: => Int) = f1 * f2

      def zero: Int = 1
    }
  }

  implicit val intMultiplicationNewType: Monoid[Int @@ Multiplication] with Enum[Int @@ Multiplication] = new Monoid[Int @@ Multiplication] with Enum[Int @@ Multiplication] {
    def append(f1: Int @@ Multiplication, f2: => Int @@ Multiplication) = Multiplication(f1 * f2)

    def zero: Int @@ Multiplication = Multiplication(1)

    def succ(b: Int @@ Multiplication) = Multiplication(Enum[Int].succ(b))

    def pred(b: Int @@ Multiplication) = Multiplication(Enum[Int].pred(b))

    override def succn(n: Int, b: Int @@ Multiplication) = Multiplication(Enum[Int].succn(n, b))

    override def predn(n: Int, b: Int @@ Multiplication) = Multiplication(Enum[Int].predn(n, b))

    override def min = Enum[Int].min map (Multiplication(_))

    override def max = Enum[Int].max map (Multiplication(_))

    def order(a1: Int @@ Multiplication, a2: Int @@ Multiplication) = Order[Int].order(a1, a2)
  }

  implicit val longInstance: Monoid[Long] with Enum[Long] with Show[Long] = new Monoid[Long] with Enum[Long] with Show[Long] {
    override def shows(f: Long) = f.toString

    def append(f1: Long, f2: => Long) = f1 + f2

    def zero: Long = 0L

    def order(x: Long, y: Long) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: Long) = b + 1
    def pred(b: Long) = b - 1
    override def succn(a: Int, b: Long) = b + a
    override def predn(a: Int, b: Long) = b - a
    override def min = Some(Long.MinValue)
    override def max = Some(Long.MaxValue)

    override def equalIsNatural: Boolean = true

    object multiplication extends Monoid[Long] {
      def append(f1: Long, f2: => Long) = f1 * f2

      def zero: Long = 1
    }

  }

  implicit val longMultiplicationNewType: Monoid[Long @@ Multiplication] with Enum[Long @@ Multiplication] = new Monoid[Long @@ Multiplication] with Enum[Long @@ Multiplication] {
    def append(f1: Long @@ Multiplication, f2: => Long @@ Multiplication) = Multiplication(f1 * f2)

    def zero: Long @@ Multiplication = Multiplication(1)

    def succ(b: Long @@ Multiplication) = Multiplication(Enum[Long].succ(b))

    def pred(b: Long @@ Multiplication) = Multiplication(Enum[Long].pred(b))

    override def succn(n: Int, b: Long @@ Multiplication) = Multiplication(Enum[Long].succn(n, b))

    override def predn(n: Int, b: Long @@ Multiplication) = Multiplication(Enum[Long].predn(n, b))

    override def min = Enum[Long].min map (Multiplication(_))

    override def max = Enum[Long].max map (Multiplication(_))

    def order(a1: Long @@ Multiplication, a2: Long @@ Multiplication) = Order[Long].order(a1, a2)
  }

  implicit val floatInstance: Order[Float] with Show[Float] = new Order[Float] with Show[Float] {
    override def shows(f: Float) = f.toString

    override def equalIsNatural: Boolean = true

    def order(x: Float, y: Float) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }

  implicit val doubleInstance: Order[Double] with Show[Double] = new Order[Double] with Show[Double] {
    override def shows(f: Double) = f.toString

    override def equalIsNatural: Boolean = true

    def order(x: Double, y: Double) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }
}

trait BooleanFunctions {

  /**
   * Conjunction. (AND)
   *
   * {{{
   * p q  p ∧ q
   * 0 0  0
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * }}}
   */
  final def conjunction(p: Boolean, q: => Boolean) = p && q

  /**
   * Disjunction. (OR)
   *
   * {{{
   * p q  p ∨ q
   * 0 0  0
   * 0 1  1
   * 1 0  1
   * 1 1  1
   * }}}
   */
  final def disjunction(p: Boolean, q: => Boolean) = p || q

  /**
   * Negation of Disjunction. (NOR)
   *
   * {{{
   * p q  p !|| q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  0
   * }}}
   */
  final def nor(p: Boolean, q: => Boolean) = !(p || q)

  /**
   * Negation of Conjunction. (NAND)
   *
   * {{{
   * p q  p !&& q
   * 0 0  1
   * 0 1  1
   * 1 0  1
   * 1 1  0
   * }}}
   */
  final def nand(p: Boolean, q: => Boolean) = !(p && q)

  /**
   * Conditional.
   *
   * {{{
   * p q  p --> q
   * 0 0  1
   * 0 1  1
   * 1 0  0
   * 1 1  1
   * }}}
   */
  final def conditional(p: Boolean, q: => Boolean) = !p || q

  /**
   * Inverse Conditional.
   *
   * {{{
   * p q  p <-- q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * }}}
   */
  final def inverseConditional(p: Boolean, q: => Boolean) = p || !q

  /**
   * Negational of Conditional.
   *
   * {{{
   * p q  p ⇏ q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * }}}
   */
  final def negConditional(p: Boolean, q: => Boolean) = p && !q

  /**
   * Negation of Inverse Conditional.
   *
   * {{{
   * p q  p <\- q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * }}}
   */
  final def negInverseConditional(p: Boolean, q: => Boolean) = !p && q


  /**
   * Executes the given side-effect if `cond` is `false`
   */
  final def unless(cond: Boolean)(f: => Unit) = if (!cond) f

  /**
   * Executes the given side-effect if `cond` is `true`
   */
  final def when(cond: Boolean)(f: => Unit) = if (cond) f

  /**
   * Returns the given argument if `cond` is `false`, otherwise, unit lifted into M.
   */
  final def unlessM[M[_], A](cond: Boolean)(f: => M[A])(implicit M: Applicative[M]): M[Unit] = if (cond) M.point(()) else M.void(f)

  /**
   * Returns the given argument if `cond` is `true`, otherwise, unit lifted into M.
   */
  final def whenM[M[_], A](cond: Boolean)(f: => M[A])(implicit M: Applicative[M]): M[Unit] = if (cond) M.void(f) else M.point(())

  /**
   * @return `t` if `cond` is `true`, `f` otherwise
   */
  final def fold[A](cond: Boolean, t: => A, f: => A): A = if (cond) t else f

  /**
   * Returns the given argument in `Some` if `cond` is `true`, `None` otherwise.
   */
  final def option[A](cond: Boolean, a: => A): Option[A] = if (cond) Some(a) else None

  /** Returns `1` if `p` is true, or `0` otherwise. */
  def test(p: Boolean): Int = if (p) 1 else 0

  /**
   * Returns the given argument if `cond` is `true`, otherwise, the zero element for the type of the given
   * argument.
   */
  final def valueOrZero[A](cond: Boolean)(value: => A)(implicit z: Monoid[A]): A = if (cond) value else z.zero

  /**
   * Returns the given argument if `cond` is `false`, otherwise, the zero element for the type of the given
   * argument.
   */
  final def zeroOrValue[A](cond: Boolean)(value: => A)(implicit z: Monoid[A]): A = if (!cond) value else z.zero

  /**
   * Returns the value `a` lifted into the context `M` if `cond` is `true`, otherwise, the empty value
   * for `M`.
   */
  final def pointOrEmpty[M[_], A](cond: Boolean)(a: => A)(implicit M: Applicative[M], M0: PlusEmpty[M]): M[A] =
    if (cond) M.point(a) else M0.empty

  /**
   * Returns the value `a` lifted into the context `M` if `cond` is `false`, otherwise, the empty value
   * for `M`.
   */
  final def emptyOrPure[M[_], A](cond: Boolean)(a: => A)(implicit M: Applicative[M], M0: PlusEmpty[M]): M[A] =
    if (!cond) M.point(a) else M0.empty

  final def pointOrEmptyNT[M[_]](cond: Boolean)(implicit M: Applicative[M], M0: PlusEmpty[M]): (Id ~> M) =
    new (Id ~> M) {
      def apply[A](a: A): M[A] = pointOrEmpty[M, A](cond)(a)
    }

  final def emptyOrPureNT[M[_]](cond: Boolean)(implicit M: Applicative[M], M0: PlusEmpty[M]): (Id ~> M) =
    new (Id ~> M) {
      def apply[A](a: A): M[A] = emptyOrPure[M, A](cond)(a)
    }
}

trait IntFunctions {
  def heaviside(i: Int) = if (i < 0) 0 else i
}

trait ShortFunctions {
  def heaviside(i: Short) = if (i < 0) 0 else i
}

trait LongFunctions {
  def heaviside(i: Long) = if (i < 0) 0 else i
}

trait DoubleFunctions {
  def heaviside(i: Double) = if (i < 0) 0 else i
}

trait FloatFunctions {
  def heaviside(i: Float) = if (i < 0) 0 else i
}

object anyVal extends AnyValInstances

object boolean extends BooleanFunctions

object short extends ShortFunctions

object int extends IntFunctions

object long extends LongFunctions

object double extends DoubleFunctions

object float extends FloatFunctions

