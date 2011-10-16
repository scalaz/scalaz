package scalaz
package std

trait AnyVals {

  implicit object unit extends Monoid[Unit] with Order[Unit] with Show[Unit] {
    def show(f: Unit): List[Char] = ().toString.toList

    def append(f1: Unit, f2: => Unit): Unit = ()

    def zero: Unit = ()

    def order(x: Unit, y: Unit): Ordering = Ordering.EQ
  }

  implicit object boolean extends Order[Boolean] with Show[Boolean] {
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

  sealed trait Disjunction

  sealed trait Conjunction

  implicit object booleanDisjunctionNewType extends Monoid[Boolean @@ Disjunction] {
    def append(f1: Boolean @@ Disjunction, f2: => Boolean @@ Disjunction) = Tag(f1 || f2)

    def zero: Boolean @@ Disjunction = Tag(false)
  }

  implicit object booleanConjunctionNewType extends Monoid[Boolean @@ Conjunction] {
    def append(f1: Boolean @@ Conjunction, f2: => Boolean @@ Conjunction) = Tag(f1 && f2)

    def zero: Boolean @@ Conjunction = Tag(true)
  }

  sealed trait Multiplication

  implicit object byte extends Monoid[Byte] with Order[Byte] with Show[Byte] {
    def show(f: Byte): List[Char] = f.toString.toList

    def append(f1: Byte, f2: => Byte): Byte = (f1 + f2).toByte

    def zero: Byte = 0

    def order(x: Byte, y: Byte): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Byte] {
      def append(f1: Byte, f2: => Byte): Byte = (f1 * f2).toByte

      def zero: Byte = 1
    }

  }

  implicit object byteMultiplicationNewType extends Monoid[Byte @@ Multiplication] {
    def append(f1: Byte @@ Multiplication, f2: => Byte @@ Multiplication): Byte @@ Multiplication = Tag((f1 * f2).toByte)

    def zero: Byte @@ Multiplication = Tag(1)
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

  implicit object charMultiplicationNewType extends Monoid[Char @@ Multiplication] {
    def append(f1: Char @@ Multiplication, f2: => Char @@ Multiplication): Char @@ Multiplication = Tag((f1 * f2).toChar)

    def zero: Char @@ Multiplication = Tag(1)
  }

  implicit object short extends Monoid[Short] with Order[Short] with Show[Short] {
    def show(f: Short): List[Char] = f.toString.toList

    def append(f1: Short, f2: => Short): Short = (f1 + f2).toShort

    def zero: Short = 0

    def order(x: Short, y: Short): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Short] {
      def append(f1: Short, f2: => Short): Short = (f1 * f2).toShort

      def zero: Short = 1
    }

  }

  implicit object shortMultiplicationNewType extends Monoid[Short @@ Multiplication] {
    def append(f1: Short @@ Multiplication, f2: => Short @@ Multiplication): Short @@ Multiplication = Tag((f1 * f2).toShort)

    def zero: Short @@ Multiplication = Tag(1)
  }

  implicit object int extends Monoid[Int] with Order[Int] with Show[Int] {
    def show(f: Int): List[Char] = f.toString.toList

    def append(f1: Int, f2: => Int): Int = f1 + f2

    def zero: Int = 0

    def order(x: Int, y: Int): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Int] {
      def append(f1: Int, f2: => Int): Int = f1 * f2

      def zero: Int = 1
    }

  }

  implicit object intMultiplicationNewType extends Monoid[Int @@ Multiplication] {
    def append(f1: Int @@ Multiplication, f2: => Int @@ Multiplication): Int @@ Multiplication = Tag(f1 * f2)

    def zero: Int @@ Multiplication = Tag(1)
  }


  implicit object long extends Monoid[Long] with Order[Long] with Show[Long] {
    def show(f: Long): List[Char] = f.toString.toList

    def append(f1: Long, f2: => Long): Long = f1 + f2

    def zero: Long = 0L

    def order(x: Long, y: Long): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    object multiplication extends Monoid[Long] {
      def append(f1: Long, f2: => Long): Long = f1 * f2

      def zero: Long = 1
    }

  }

  implicit object longMultiplicationNewType extends Monoid[Long @@ Multiplication] {
    def append(f1: Long @@ Multiplication, f2: => Long @@ Multiplication): Long @@ Multiplication = Tag(f1 * f2)

    def zero: Long @@ Multiplication = Tag(1)
  }

  implicit object float extends Monoid[Float] with Order[Float] with Show[Float] {
    def show(f: Float): List[Char] = f.toString.toList

    def append(f1: Float, f2: => Float): Float = f1 + f2

    def zero: Float = 0f

    def order(x: Float, y: Float): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }

  implicit object double extends Monoid[Double] with Order[Double] with Show[Double] {
    def show(f: Double): List[Char] = f.toString.toList

    def append(f1: Double, f2: => Double): Double = f1 + f2

    def zero: Double = 0d

    def order(x: Double, y: Double): Ordering = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT
  }

}

object AnyVal extends AnyVals