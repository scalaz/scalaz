package scalaz

/**
 * A type safe alternative to   { @scala.Any.= = }.
 * <p/>
 * For example:
 * <pre>
 *   (1 ≟ 0) ≟ false 
 *   (List("1") ≟ List("1")) ≟ true
 *   (1 ≟ "1") // compile error
 *   (1 ≟ 0L) // compile error
 * </pre>
 * @see scalaz.Identity # ≟
 * @see scalaz.Identity # ≠
 */
trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

trait Equals {
  import Scalaz._
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  /**
   * Constructs an `Equal` instance for type `A` based on `Any.==`.
   */
  def equalA[A]: Equal[A] = equal[A](_ == _)

  def equalBy[A, B: Equal](f: A => B): Equal[A] = implicitly[Equal[B]] ∙ f
}

object Equal {
  import Scalaz._
  import java.math.BigInteger
  import xml.NodeSeq
  import Predef.{implicitly => i}

  implicit def DigitEqual: Equal[Digit] = equalA

  implicit def OrderingEqual: Equal[Ordering] = equalA

  implicit def StringEqual: Equal[String] = equalA

  def NewTypeEqual[B: Equal, A <: NewType[B]]: Equal[A] = equalBy(_.value)

  implicit def UnitEqual: Equal[Unit] = equalA

  implicit def IntEqual: Equal[Int] = equalA

  implicit def ByteEqual: Equal[Byte] = equalA

  implicit def BooleanEqual: Equal[Boolean] = equalA

  implicit def CharEqual: Equal[Char] = equalA

  implicit def LongEqual: Equal[Long] = equalA

  implicit def ShortEqual: Equal[Short] = equalA

  implicit def FloatEqual: Equal[Float] = equalA

  implicit def DoubleEqual: Equal[Double] = equalA

  implicit def IntMultiplicationEqual: Equal[IntMultiplication] = NewTypeEqual[Int, IntMultiplication]

  implicit def BooleanConjunctionEqual: Equal[BooleanConjunction] = NewTypeEqual[Boolean, BooleanConjunction]

  implicit def CharMultiplicationEqual: Equal[CharMultiplication] = NewTypeEqual[Char, CharMultiplication]

  implicit def ByteMultiplicationEqual: Equal[ByteMultiplication] = NewTypeEqual[Byte, ByteMultiplication]

  implicit def LongMultiplicationEqual: Equal[LongMultiplication] = NewTypeEqual[Long, LongMultiplication]

  implicit def ShortMultiplicationEqual: Equal[ShortMultiplication] = NewTypeEqual[Short, ShortMultiplication]

  implicit def BigIntegerEqual: Equal[BigInteger] = equalA[java.math.BigInteger]

  implicit def BigIntegerMultiplicationEqual: Equal[BigIntegerMultiplication] = NewTypeEqual[BigInteger, BigIntegerMultiplication]

  implicit def BigIntEqual: Equal[BigInt] = equalA

  implicit def BigIntMultiplicationEqual: Equal[BigIntMultiplication] = NewTypeEqual[BigInt, BigIntMultiplication]

  implicit def NodeSeqEqual: Equal[NodeSeq] = equalA

  implicit def NonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] = equalBy(_.list)

  implicit def ZipStreamEqual[A: Equal]: Equal[ZipStream[A]] = equalBy(_.value)

  implicit def Tuple1Equal[A: Equal]: Equal[Tuple1[A]] = equalBy(_._1)

  implicit def Tuple2Equal[A: Equal, B: Equal]: Equal[(A, B)] = equal {
    case ((a1, b1), (a2, b2)) => a1 ≟ a2 && b1 ≟ b2
  }

  implicit def Tuple3Equal[A: Equal, B: Equal, C: Equal]: Equal[(A, B, C)] = equal {
    case ((a1, b1, c1), (a2, b2, c2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2
  }

  implicit def Tuple4Equal[A: Equal, B: Equal, C: Equal, D: Equal]: Equal[(A, B, C, D)] = equal {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2
  }

  implicit def Tuple5Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal]: Equal[(A, B, C, D, E)] = equal {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2
  }

  implicit def Tuple6Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal]: Equal[(A, B, C, D, E, F)] = equal {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2 && f1 ≟ f2
  }

  implicit def Tuple7Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal]: Equal[(A, B, C, D, E, F, G)] = equal {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2 && f1 ≟ f2 && g1 ≟ g2
  }

  implicit def Function0Equal[A: Equal]: Equal[Function0[A]] = equalBy(_.apply)

  implicit def OptionEqual[A: Equal]: Equal[Option[A]] = equal {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  }

  implicit def OptionFirstEqual[A: Equal]: Equal[FirstOption[A]] = equalBy(_.value)

  implicit def OptionLastEqual[A: Equal]: Equal[LastOption[A]] = equalBy(_.value)

  implicit def EitherEqual[A: Equal, B: Equal]: Equal[Either[A, B]] = equal {
    case (Left(a1), Left(a2)) => a1 ≟ a2
    case (Right(b1), Right(b2)) => b1 ≟ b2
    case _ => false
  }

  implicit def EitherLeftEqual[A: Equal, X]: Equal[Either.LeftProjection[A, X]] = equal((a1, a2) =>
    (a1.toOption, a2.toOption) match {
      case (Some(a1), Some(a2)) => a1 ≟ a2
      case (a1, a2) => a1.isDefined == a2.isDefined
    })

  implicit def EitherRightEqual[X, A: Equal]: Equal[Either.RightProjection[X, A]] = equal((a1, a2) =>
    (a1.toOption, a2.toOption) match {
      case (Some(a1), Some(a2)) => a1 ≟ a2
      case (a1, a2) => a1.isDefined == a2.isDefined
    })

  implicit def ValidationEqual[E: Equal, A: Equal]: Equal[Validation[E, A]] = equalBy(_.either)

  implicit def FailProjectionEqual[E: Equal, A: Equal]: Equal[FailProjection[E, A]] = equalBy(_.validation)

  implicit def TreeEqual[A: Equal]: Equal[Tree[A]] =
    equal[Tree[A]]((a1, a2) =>
      a1.rootLabel ≟ a2.rootLabel
              && i[Equal[Iterable[Tree[A]]]].equal(a1.subForest, a2.subForest))

  implicit def TreeLocEqual[A: Equal]: Equal[TreeLoc[A]] = {
    equal[TreeLoc[A]]((a1, a2) =>
      a1.tree ≟ a2.tree
              && a1.lefts ≟ a2.lefts && a1.rights ≟ a2.rights && a1.parents ≟ a2.parents)
  }

  import concurrent.Promise
  implicit def PromiseEqual[A: Equal]: Equal[Promise[A]] =
    equal[Promise[A]]((a1, a2) => a1.get ≟ a2.get)
  
  implicit def TraversableEqual[CC[X] <: collection.TraversableLike[X, CC[X]] with Traversable[X] : CanBuildAnySelf, A: Equal]: Equal[CC[A]] = new Equal[CC[A]] {
    def equal(a: CC[A], b: CC[A]) = {
      import Scalaz._
      // This is a horribly inefficient, but general way to implement equality with respect to the element equality for an unknown
      // collection type CC. It ensures that Set(1, 2, 3) === Set(3, 2, 1). We should move this to EqualLow, and add specific
      // implementations of Equal for common collection types, such as List, Stream, Vector, and Array.
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[A, A]
      case class EqualWrap[A: Equal](a: A) {
        override def equals(other: Any) = other matchOrZero {
          case EqualWrap(b) => a === b.asInstanceOf[A]
        }
      }
      a.map(new EqualWrap(_)) == a.map(new EqualWrap(_))
    }
  }

  def IterableEqual[CC[X] <: Iterable[X], A: Equal]: Equal[CC[A]] = equal((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 ≠ x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def MapEqual[CC[K, V] <: collection.Map[K, V], A: Equal, B: Equal]: Equal[collection.Map[A, B]] =
    i[Equal[Iterable[(A, B)]]] covary

  implicit def JavaIterableEqual[A: Equal]: Equal[java.lang.Iterable[A]] = equal((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 ≠ x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def JavaMapEntry[K: Equal, V: Equal]: Equal[java.util.Map.Entry[K, V]] = equal((a1, a2) => a1.getKey ≟ a2.getKey)

  implicit def JavaMapEqual[K: Equal, V: Equal]: Equal[java.util.Map[K, V]] = equalBy(_.entrySet)

  implicit def CallableEqual[A: Equal]: Equal[java.util.concurrent.Callable[A]] = equalBy(_.call)

  implicit def ZipperEqual[A: Equal]: Equal[Zipper[A]] = equalBy(_.stream)
}
