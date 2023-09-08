package scalaz
package std

import scalaz._
import Id._

trait AnyValInstances {

  implicit val unitInstance: Monoid[Unit] with Enum[Unit] with Show[Unit] with SemiLattice[Unit] = new Monoid[Unit] with Enum[Unit] with Show[Unit] with SemiLattice[Unit] {
    override def shows(f: Unit) = ().toString

    def append(f1: Unit, f2: => Unit) = ()

    def zero = ()

    def order(x: Unit, y: Unit): Ordering = Ordering.EQ

    def succ(u: Unit) = ()

    def pred(u: Unit) = ()

    override def succn(a: Int, b: Unit) = ()

    override def predn(a: Int, b: Unit) = ()

    override def min: Option[Unit] = Some(())

    override def max: Option[Unit] = Some(())

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

  implicit val booleanDisjunctionNewTypeInstance: Monoid[Boolean @@ Disjunction] with Enum[Boolean @@ Disjunction] with Band[Boolean @@ Disjunction] = new Monoid[Boolean @@ Disjunction] with Enum[Boolean @@ Disjunction] with Band[Boolean @@ Disjunction] {
    def append(f1: Boolean @@ Disjunction, f2: => Boolean @@ Disjunction) = Disjunction(Tag.unwrap(f1) || Tag.unwrap(f2))

    def zero: Boolean @@ Disjunction = Disjunction(false)

    def order(a1: Boolean @@ Disjunction, a2: Boolean @@ Disjunction) = Order[Boolean].order(Tag.unwrap(a1), Tag.unwrap(a2))

    def succ(b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].succ(Tag.unwrap(b)))

    def pred(b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Boolean @@ Disjunction) = Disjunction(Enum[Boolean].predn(n, Tag.unwrap(b)))

    override def min = Disjunction.subst(Enum[Boolean].min)

    override def max = Disjunction.subst(Enum[Boolean].max)

  }

  implicit val booleanConjunctionNewTypeInstance: Monoid[Boolean @@ Conjunction] with Enum[Boolean @@ Conjunction] with Band[Boolean @@ Conjunction] = new Monoid[Boolean @@ Conjunction] with Enum[Boolean @@ Conjunction] with Band[Boolean @@ Conjunction] {
    def append(f1: Boolean @@ Conjunction, f2: => Boolean @@ Conjunction) = Conjunction(Tag.unwrap(f1) && Tag.unwrap(f2))

    def zero: Boolean @@ Conjunction = Conjunction(true)

    def order(a1: Boolean @@ Conjunction, a2: Boolean @@ Conjunction) = Order[Boolean].order(Tag.unwrap(a1), Tag.unwrap(a2))

    def succ(b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].succ(Tag.unwrap(b)))

    def pred(b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Boolean @@ Conjunction) = Conjunction(Enum[Boolean].predn(n, Tag.unwrap(b)))

    override def min = Conjunction.subst(Enum[Boolean].min)

    override def max = Conjunction.subst(Enum[Boolean].max)

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
    override def min: Option[Byte] = Some(Byte.MinValue)
    override def max: Option[Byte] = Some(Byte.MaxValue)

    override def equalIsNatural: Boolean = true
  }

  import Tags.{Multiplication}

  implicit val byteMultiplicationNewType: Monoid[Byte @@ Multiplication] with Enum[Byte @@ Multiplication] = new Monoid[Byte @@ Multiplication] with Enum[Byte @@ Multiplication] {
    def append(f1: Byte @@ Multiplication, f2: => Byte @@ Multiplication) = Multiplication((Tag.unwrap(f1) * Tag.unwrap(f2)).toByte)

    def zero: Byte @@ Multiplication = Multiplication(1)

    def order(a1: Byte @@ Multiplication, a2: Byte @@ Multiplication) = Order[Byte].order(Tag.unwrap(a1), Tag.unwrap(a2))

    def succ(b: Byte @@ Multiplication) = Multiplication(Enum[Byte].succ(Tag.unwrap(b)))

    def pred(b: Byte @@ Multiplication) = Multiplication(Enum[Byte].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Byte @@ Multiplication) = Multiplication(Enum[Byte].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Byte @@ Multiplication) = Multiplication(Enum[Byte].predn(n, Tag.unwrap(b)))

    override def min = Multiplication.subst(Enum[Byte].min)

    override def max = Multiplication.subst(Enum[Byte].max)

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
    override def min: Option[Char] = Some(Char.MinValue)
    override def max: Option[Char] = Some(Char.MaxValue)

    override def equalIsNatural: Boolean = true
  }

  implicit val charMultiplicationNewType: Monoid[Char @@ Multiplication] with Enum[Char @@ Multiplication] = new Monoid[Char @@ Multiplication] with Enum[Char @@ Multiplication] {
    def append(f1: Char @@ Multiplication, f2: => Char @@ Multiplication) = Multiplication((Tag.unwrap(f1) * Tag.unwrap(f2)).toChar)

    def zero: Char @@ Multiplication = Multiplication(1)

    def order(a1: Char @@ Multiplication, a2: Char @@ Multiplication) = Order[Char].order(Tag.unwrap(a1), Tag.unwrap(a2))

    def succ(b: Char @@ Multiplication) = Multiplication(Enum[Char].succ(Tag.unwrap(b)))

    def pred(b: Char @@ Multiplication) = Multiplication(Enum[Char].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Char @@ Multiplication) = Multiplication(Enum[Char].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Char @@ Multiplication) = Multiplication(Enum[Char].predn(n, Tag.unwrap(b)))

    override def min = Multiplication.subst(Enum[Char].min)

    override def max = Multiplication.subst(Enum[Char].max)

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
    override def min: Option[Short] = Some(Short.MinValue)
    override def max: Option[Short] = Some(Short.MaxValue)

    override def equalIsNatural: Boolean = true
  }

  implicit val shortMultiplicationNewType: Monoid[Short @@ Multiplication] with Enum[Short @@ Multiplication] = new Monoid[Short @@ Multiplication] with Enum[Short @@ Multiplication] {
    def append(f1: Short @@ Multiplication, f2: => Short @@ Multiplication) = Multiplication((Tag.unwrap(f1) * Tag.unwrap(f2)).toShort)

    def zero: Short @@ Multiplication = Multiplication(1)

    def succ(b: Short @@ Multiplication) = Multiplication(Enum[Short].succ(Tag.unwrap(b)))

    def pred(b: Short @@ Multiplication) = Multiplication(Enum[Short].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Short @@ Multiplication) = Multiplication(Enum[Short].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Short @@ Multiplication) = Multiplication(Enum[Short].predn(n, Tag.unwrap(b)))

    override def min = Multiplication.subst(Enum[Short].min)

    override def max = Multiplication.subst(Enum[Short].max)

    def order(a1: Short @@ Multiplication, a2: Short @@ Multiplication) = Order[Short].order(Tag.unwrap(a1), Tag.unwrap(a2))
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
    override def min: Option[Int] = Some(Int.MinValue)
    override def max: Option[Int] = Some(Int.MaxValue)

    override def equalIsNatural: Boolean = true
  }

  implicit val intMultiplicationNewType: Monoid[Int @@ Multiplication] with Enum[Int @@ Multiplication] = new Monoid[Int @@ Multiplication] with Enum[Int @@ Multiplication] {
    def append(f1: Int @@ Multiplication, f2: => Int @@ Multiplication) = Multiplication(Tag.unwrap(f1) * Tag.unwrap(f2))

    def zero: Int @@ Multiplication = Multiplication(1)

    def succ(b: Int @@ Multiplication) = Multiplication(Enum[Int].succ(Tag.unwrap(b)))

    def pred(b: Int @@ Multiplication) = Multiplication(Enum[Int].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Int @@ Multiplication) = Multiplication(Enum[Int].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Int @@ Multiplication) = Multiplication(Enum[Int].predn(n, Tag.unwrap(b)))

    override def min = Multiplication.subst(Enum[Int].min)

    override def max = Multiplication.subst(Enum[Int].max)

    def order(a1: Int @@ Multiplication, a2: Int @@ Multiplication) = Order[Int].order(Tag.unwrap(a1), Tag.unwrap(a2))
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
    override def min: Option[Long] = Some(Long.MinValue)
    override def max: Option[Long] = Some(Long.MaxValue)

    override def equalIsNatural: Boolean = true
  }

  implicit val longMultiplicationNewType: Monoid[Long @@ Multiplication] with Enum[Long @@ Multiplication] = new Monoid[Long @@ Multiplication] with Enum[Long @@ Multiplication] {
    def append(f1: Long @@ Multiplication, f2: => Long @@ Multiplication) = Multiplication(Tag.unwrap(f1) * Tag.unwrap(f2))

    def zero: Long @@ Multiplication = Multiplication(1)

    def succ(b: Long @@ Multiplication) = Multiplication(Enum[Long].succ(Tag.unwrap(b)))

    def pred(b: Long @@ Multiplication) = Multiplication(Enum[Long].pred(Tag.unwrap(b)))

    override def succn(n: Int, b: Long @@ Multiplication) = Multiplication(Enum[Long].succn(n, Tag.unwrap(b)))

    override def predn(n: Int, b: Long @@ Multiplication) = Multiplication(Enum[Long].predn(n, Tag.unwrap(b)))

    override def min = Multiplication.subst(Enum[Long].min)

    override def max = Multiplication.subst(Enum[Long].max)

    def order(a1: Long @@ Multiplication, a2: Long @@ Multiplication) = Order[Long].order(Tag.unwrap(a1), Tag.unwrap(a2))
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
  final def conjunction(p: Boolean, q: => Boolean): Boolean = p && q

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
  final def disjunction(p: Boolean, q: => Boolean): Boolean = p || q

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
  final def nor(p: Boolean, q: => Boolean): Boolean = !(p || q)

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
  final def nand(p: Boolean, q: => Boolean): Boolean = !(p && q)

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
  final def conditional(p: Boolean, q: => Boolean): Boolean = !p || q

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
  final def inverseConditional(p: Boolean, q: => Boolean): Boolean = p || !q

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
  final def negConditional(p: Boolean, q: => Boolean): Boolean = p && !q

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
  final def negInverseConditional(p: Boolean, q: => Boolean): Boolean = !p && q


  /**
   * Executes the given side-effect if `cond` is `false`
   */
  final def unless(cond: Boolean)(f: => Unit): Unit = if (!cond) f

  /**
   * Executes the given side-effect if `cond` is `true`
   */
  final def when(cond: Boolean)(f: => Unit): Unit = if (cond) f

  /**
   * Returns the given argument if `cond` is `false`, otherwise, unit lifted into M.
   */
  final def unlessM[M[_], A](cond: Boolean)(f: => M[A])(implicit M: Applicative[M]): M[Unit] = M.unlessM(cond)(f)

  /** A version of `unlessM` that infers the type constructor `M`. */
  final def unlessMU[MA](cond: Boolean)(f: => MA)(implicit M: Unapply[Applicative, MA]): M.M[Unit] = M.TC.unlessM(cond)(M(f))

  /**
   * Returns the given argument if `cond` is `true`, otherwise, unit lifted into M.
   */
  final def whenM[M[_], A](cond: Boolean)(f: => M[A])(implicit M: Applicative[M]): M[Unit] = M.whenM(cond)(f)

  /** A version of `whenM` that infers the type constructor `M`. */
  final def whenMU[MA](cond: Boolean)(f: => MA)(implicit M: Unapply[Applicative, MA]): M.M[Unit] = M.TC.whenM(cond)(M(f))

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
  def heaviside(i: Int):Int = if (i < 0) 0 else 1
}

trait ShortFunctions {
  def heaviside(i: Short):Short = if (i < 0) 0 else 1
}

trait LongFunctions {
  def heaviside(i: Long):Long = if (i < 0) 0 else 1
}

trait DoubleFunctions {
  def heaviside(i: Double):Double = if (i < 0) 0 else 1.0
}

trait FloatFunctions {
  def heaviside(i: Float):Float = if (i < 0) 0 else 1.0f
}

object anyVal extends AnyValInstances

object boolean extends BooleanFunctions

object short extends ShortFunctions

object int extends IntFunctions

object long extends LongFunctions

object double extends DoubleFunctions

object float extends FloatFunctions

