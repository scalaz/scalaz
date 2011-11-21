package scalaz
package std

import scalaz._

trait AnyValInstances {

  implicit object unitInstance extends Group[Unit] with Order[Unit] with Show[Unit] {
    def show(f: Unit): List[Char] = ().toString.toList

    def append(f1: Unit, f2: => Unit): Unit = ()

    def zero: Unit = ()

    def inverse(f:Unit):Unit = ()

    def order(x: Unit, y: Unit): Ordering = Ordering.EQ
  }

  implicit object booleanInstance extends Order[Boolean] with Show[Boolean] {
    def show(f: Boolean): List[Char] = f.toString.toList

    def order(x: Boolean, y: Boolean): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object conjunction extends Monoid[Boolean] {
      def append(f1: Boolean, f2: => Boolean): Boolean = f1 && f2

      def zero: Boolean = true
    }

    object disjunction extends Monoid[Boolean] {
      def append(f1: Boolean, f2: => Boolean): Boolean = f1 || f2

      def zero: Boolean = false
    }

  }

  import Tags.{Conjunction, Disjunction}

  implicit object booleanDisjunctionNewTypeInstance extends Monoid[Boolean @@ Disjunction] with Order[Boolean @@ Disjunction] {
    def append(f1: Boolean @@ Disjunction, f2: => Boolean @@ Disjunction) = Tag(f1 || f2)

    def zero: Boolean @@ Disjunction = Tag(false)

    def order(a1: Boolean @@ Disjunction, a2: Boolean @@ Disjunction) = Order[Boolean].order(a1, a2)
  }

  implicit object booleanConjunctionNewTypeInstance extends Monoid[Boolean @@ Conjunction] with Order[Boolean @@ Conjunction] {
    def append(f1: Boolean @@ Conjunction, f2: => Boolean @@ Conjunction) = Tag(f1 && f2)

    def zero: Boolean @@ Conjunction = Tag(true)

    def order(a1: Boolean @@ Conjunction, a2: Boolean @@ Conjunction) = Order[Boolean].order(a1, a2)

  }

  implicit object byteInstance extends Monoid[Byte] with Order[Byte] with Show[Byte] {
    def show(f: Byte): List[Char] = f.toString.toList

    def append(f1: Byte, f2: => Byte): Byte = (f1 + f2).toByte

    def zero: Byte = 0

    def order(x: Byte, y: Byte): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Byte] {
      def append(f1: Byte, f2: => Byte): Byte = (f1 * f2).toByte

      def zero: Byte = 1
    }

  }

  import Tags.{Multiplication}

  implicit object byteMultiplicationNewType extends Monoid[Byte @@ Multiplication] with Order[Byte @@ Multiplication] {
    def append(f1: Byte @@ Multiplication, f2: => Byte @@ Multiplication): Byte @@ Multiplication = Tag((f1 * f2).toByte)

    def zero: Byte @@ Multiplication = Tag(1)

    def order(a1: Byte @@ Multiplication, a2: Byte @@ Multiplication) = Order[Byte].order(a1, a2)

  }

  implicit object char extends Monoid[Char] with Order[Char] with Show[Char] {
    def show(f: Char): List[Char] = f.toString.toList

    def append(f1: Char, f2: => Char): Char = (f1 + f2).toChar

    def zero: Char = 0

    def order(x: Char, y: Char): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Char] {
      def append(f1: Char, f2: => Char): Char = (f1 * f2).toChar

      def zero: Char = 1
    }

  }

  implicit object charMultiplicationNewType extends Monoid[Char @@ Multiplication] with Order[Char @@ Multiplication] {
    def append(f1: Char @@ Multiplication, f2: => Char @@ Multiplication): Char @@ Multiplication = Tag((f1 * f2).toChar)

    def zero: Char @@ Multiplication = Tag(1)

    def order(a1: Char @@ Multiplication, a2: Char @@ Multiplication) = Order[Char].order(a1, a2)
  }

  implicit object shortInstance extends Group[Short] with Order[Short] with Show[Short] {
    def show(f: Short): List[Char] = f.toString.toList

    def append(f1: Short, f2: => Short): Short = (f1 + f2).toShort

    def zero: Short = 0

    def inverse(f:Short):Short = (-f).toShort

    def order(x: Short, y: Short): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Short] {
      def append(f1: Short, f2: => Short): Short = (f1 * f2).toShort

      def zero: Short = 1
    }

  }

  implicit object shortMultiplicationNewType extends Monoid[Short @@ Multiplication] with Order[Short @@ Multiplication] {
    def append(f1: Short @@ Multiplication, f2: => Short @@ Multiplication): Short @@ Multiplication = Tag((f1 * f2).toShort)

    def zero: Short @@ Multiplication = Tag(1)

    def order(a1: Short @@ Multiplication, a2: Short @@ Multiplication) = Order[Short].order(a1, a2)
  }

  implicit object intInstance extends Group[Int] with Order[Int] with Show[Int] {
    def show(f: Int): List[Char] = f.toString.toList

    def append(f1: Int, f2: => Int): Int = f1 + f2

    def zero: Int = 0

    def inverse(f:Int):Int = -f

    def order(x: Int, y: Int): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Int] {
      def append(f1: Int, f2: => Int): Int = f1 * f2

      def zero: Int = 1
    }
  }

  implicit object intMultiplicationNewType extends Monoid[Int @@ Multiplication] with Order[Int @@ Multiplication] {
    def append(f1: Int @@ Multiplication, f2: => Int @@ Multiplication): Int @@ Multiplication = Tag(f1 * f2)

    def zero: Int @@ Multiplication = Tag(1)

    def order(a1: Int @@ Multiplication, a2: Int @@ Multiplication) = Order[Int].order(a1, a2)
  }

  implicit object longInstance extends Group[Long] with Order[Long] with Show[Long] {
    def show(f: Long): List[Char] = f.toString.toList

    def append(f1: Long, f2: => Long): Long = f1 + f2

    def zero: Long = 0L

    def inverse(f: Long): Long = -f

    def order(x: Long, y: Long): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Long] {
      def append(f1: Long, f2: => Long): Long = f1 * f2

      def zero: Long = 1
    }

  }

  implicit object longMultiplicationNewType extends Monoid[Long @@ Multiplication] with Order[Long @@ Multiplication] {
    def append(f1: Long @@ Multiplication, f2: => Long @@ Multiplication): Long @@ Multiplication = Tag(f1 * f2)

    def zero: Long @@ Multiplication = Tag(1)

    def order(a1: Long @@ Multiplication, a2: Long @@ Multiplication) = Order[Long].order(a1, a2)
  }

  implicit object floatInstance extends Group[Float] with Order[Float] with Show[Float] {
    def show(f: Float): List[Char] = f.toString.toList

    def append(f1: Float, f2: => Float): Float = f1 + f2

    def zero: Float = 0f

    def inverse(f: Float): Float = -f

    def order(x: Float, y: Float): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }

  implicit object floatMultiplicationNewType extends Group[Float @@ Multiplication] {
    def append(f1: Float @@ Multiplication, f2: => Float @@ Multiplication): Float @@ Multiplication = Tag(f1 * f2)

    def zero: Float @@ Multiplication = Tag(1.0f)

    def inverse(f: Float @@ Multiplication): Float @@ Multiplication = Tag(1.0f/f)

  }

  implicit object doubleInstance extends Group[Double] with Order[Double] with Show[Double] {
    def show(f: Double): List[Char] = f.toString.toList

    def append(f1: Double, f2: => Double): Double = f1 + f2

    def zero: Double = 0d

    def inverse(f: Double): Double = -f

    def order(x: Double, y: Double): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }

  implicit object doubleMultiplicationNewType extends Group[Double @@ Multiplication] {
    def append(f1: Double @@ Multiplication, f2: => Double @@ Multiplication): Double @@ Multiplication = Tag(f1 * f2)

    def zero: Double @@ Multiplication = Tag(1.0d)

    def inverse(f: Double @@ Multiplication): Double @@ Multiplication = Tag(1.0d/f)

  }

}

trait BooleanFunctions {

  /**
   * Conjunction. (AND)
   *
   * <pre>
   * p q  p ∧ q
   * 0 0  0
   * 0 1  0
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  final def conjunction(p: Boolean, q: => Boolean) = p && q

  /**
   * Disjunction. (OR)
   *
   * <pre>
   * p q  p ∨ q
   * 0 0  0
   * 0 1  1
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  final def disjunction(p: Boolean, q: => Boolean) = p || q

  /**
   * Negation of Conjunction. (NOR)
   *
   * <pre>
   * p q  p !&& q
   * 0 0  1
   * 0 1  1
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  final def nor(p: Boolean, q: => Boolean) = !p || !q

  /**
   * Negation of Disjunction. (NAND)
   *
   * <pre>
   * p q  p !|| q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  final def nand(p: Boolean, q: => Boolean) = !p && !q

  /**
   * Conditional.
   *
   * <pre>
   * p q  p --> q
   * 0 0  1
   * 0 1  1
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  final def conditional(p: Boolean, q: => Boolean) = !p || q

  /**
   * Inverse Conditional.
   *
   * <pre>
   * p q  p <-- q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  final def inverseConditional(p: Boolean, q: => Boolean) = p || !q

  /**
   * Negational of Conditional.
   *
   * <pre>
   * p q  p ⇏ q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  final def negConditional(p: Boolean, q: => Boolean) = p && !q

  /**
   * Negation of Inverse Conditional.
   *
   * <pre>
   * p q  p <\- q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  final def negInverseConditional(p: Boolean, q: => Boolean) = !p && q


  /**
   * Executes the given side-effect if `cond` is <code>false</code>.
   */
  final def unless(cond: Boolean)(f: => Unit) = if (!cond) f

  /**
   * Executes the given side-effect if `cond` is <code>true</code>.
   */
  final def when(cond: Boolean)(f: => Unit) = if (cond) f

  /**
   * @return `t` if `cond` is `true`, `f` otherwise
   */
  final def fold[A](cond: Boolean, t: => A, f: => A): A = if (cond) t else f

  /**
   * Returns the given argument in <code>Some</code> if `cond` is `true`, `None` otherwise.
   */
  final def option[A](cond: Boolean, a: => A): Option[A] = if (cond) Some(a) else None

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
  final def pointOrEmpty[M[_], A](cond: Boolean)(a: => A)(implicit M: Pointed[M], M0: Empty[M]): M[A] =
    if (cond) M.point(a) else M0.empty

  /**
   * Returns the value `a` lifted into the context `M` if `cond` is `false`, otherwise, the empty value
   * for `M`.
   */
  final def emptyOrPure[M[_], A](cond: Boolean)(a: => A)(implicit M: Pointed[M], M0: Empty[M]): M[A] =
    if (!cond) M.point(a) else M0.empty

  final def pointOrEmptyNT[M[_]](cond: Boolean)(implicit M: Pointed[M], M0: Empty[M]): (Id ~> M) =
    new (Id ~> M) {
      def apply[A](a: A): M[A] = pointOrEmpty(cond)(a)
    }

  final def emptyOrPureNT[M[_]](cond: Boolean)(implicit M: Pointed[M], M0: Empty[M]): (Id ~> M) =
    new (Id ~> M) {
      def apply[A](a: A): M[A] = emptyOrPure(cond)(a)
    }
}

object anyVal extends AnyValInstances

object boolean extends BooleanFunctions
