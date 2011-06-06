package scalaz

sealed trait Equal[A] {
  val equal: A => A => Boolean

  def unequal: A => A => Boolean =
    a1 => a2 => !equal(a1)(a2)

  def contramap[B](f: B => A): Equal[B] =
    Equal.equal(b1 => b2 => Equal.this.equal(f(b1))(f(b2)))
}

object Equal extends Equals

trait Equals {
  def equal[A](f: A => A => Boolean): Equal[A] = new Equal[A] {
    val equal = f
  }

  def equalC[A](f: (A, A) => Boolean): Equal[A] =
    equal(a1 => a2 => f(a1, a2))

  private trait NaturalEqual

  /**
   * Constructs an `Equal` instance for type `A` based on `Any.==`.
   */
  def equalA[A]: Equal[A] = new Equal[A] with NaturalEqual {
    val equal = (a1: A) => (a2: A) => a1 == a2
  }

  def equalBy[A, B: Equal](f: A => B): Equal[A] =
    implicitly[Equal[B]] contramap f

  def orderEqual[A](implicit o: Order[A]): Equal[A] = new Equal[A] {
    val equal = (a1: A) => (a2: A) => o.order(a1)(a2) == EQ
  }

  implicit def EqualContravariant: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](f: B => A) =
      r => equal[B](b1 => b2 => r.equal(f(b1))(f(b2)))
  }

  import Predef.{implicitly => i}

  implicit def ThrowableEqual: Equal[Throwable] = equalA

  implicit def StringEqual: Equal[String] = equalA

  implicit def SymbolEqual: Equal[Symbol] = equalA

  implicit def UnitEqual: Equal[Unit] = equalA

  implicit def BooleanEqual: Equal[Boolean] = equalA

  implicit def ByteEqual: Equal[Byte] = equalA

  implicit def CharEqual: Equal[Char] = equalA

  implicit def IntEqual: Equal[Int] = equalA

  implicit def LongEqual: Equal[Long] = equalA

  implicit def ShortEqual: Equal[Short] = equalA

  implicit def FloatEqual: Equal[Float] = equalA

  implicit def DoubleEqual: Equal[Double] = equalA

  def UnpackEqual[T, R](implicit s: Equal[R], u: ^*^[T, R]): Equal[T] =
    implicitly[Equal[R]] contramap (u.unpack(_))

  implicit def BigIntegerEqual: Equal[java.math.BigInteger] = equalA[java.math.BigInteger]

  implicit def BigIntEqual: Equal[BigInt] = equalA

  implicit def NodeSeqEqual: Equal[xml.NodeSeq] = equalA

  def IterableEqual[CC[X] <: Iterable[X], A: Equal]: Equal[CC[A]] =
    equal(a1 => a2 => {
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = false

      while (i1.hasNext && i2.hasNext && !b) {
        val x1 = i1.next
        val x2 = i2.next

        if (implicitly[Equal[A]].unequal(x1)(x2)) {
          b = true
        }
      }

      !(b || i1.hasNext || i2.hasNext)
    })

  implicit def Function0Equal[A: Equal]: Equal[Function0[A]] = equalBy(_.apply)

  implicit def Tuple1Equal[A: Equal]: Equal[Tuple1[A]] = equalBy(_._1)

  implicit def Tuple2Equal[A: Equal, B: Equal]: Equal[(A, B)] =
    equalC {
      case ((a1, b1), (a2, b2)) => implicitly[Equal[A]].equal(a1)(a2) && implicitly[Equal[B]].equal(b1)(b2)
    }

  implicit def Tuple3Equal[A: Equal, B: Equal, C: Equal]: Equal[(A, B, C)] =
    equalC {
      case ((a1, b1, c1), (a2, b2, c2)) => implicitly[Equal[A]].equal(a1)(a2) && implicitly[Equal[B]].equal(b1)(b2) && implicitly[Equal[C]].equal(c1)(c2)
    }

  implicit def Tuple4Equal[A: Equal, B: Equal, C: Equal, D: Equal]: Equal[(A, B, C, D)] =
    equalC {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => implicitly[Equal[A]].equal(a1)(a2) && implicitly[Equal[B]].equal(b1)(b2) && implicitly[Equal[C]].equal(c1)(c2) && implicitly[Equal[D]].equal(d1)(d2)
    }

  implicit def Tuple5Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal]: Equal[(A, B, C, D, E)] =
    equalC {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => implicitly[Equal[A]].equal(a1)(a2) && implicitly[Equal[B]].equal(b1)(b2) && implicitly[Equal[C]].equal(c1)(c2) && implicitly[Equal[D]].equal(d1)(d2) && implicitly[Equal[E]].equal(e1)(e2)
    }

  implicit def Tuple6Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal]: Equal[(A, B, C, D, E, F)] =
    equalC {
      case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => implicitly[Equal[A]].equal(a1)(a2) && implicitly[Equal[B]].equal(b1)(b2) && implicitly[Equal[C]].equal(c1)(c2) && implicitly[Equal[D]].equal(d1)(d2) && implicitly[Equal[E]].equal(e1)(e2) && implicitly[Equal[F]].equal(f1)(f2)
    }

  implicit def Tuple7Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal]: Equal[(A, B, C, D, E, F, G)] =
    equalC {
      case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => implicitly[Equal[A]].equal(a1)(a2) && implicitly[Equal[B]].equal(b1)(b2) && implicitly[Equal[C]].equal(c1)(c2) && implicitly[Equal[D]].equal(d1)(d2) && implicitly[Equal[E]].equal(e1)(e2) && implicitly[Equal[F]].equal(f1)(f2) && implicitly[Equal[G]].equal(g1)(g2)
    }

  implicit def OptionEqual[A: Equal]: Equal[Option[A]] =
    equalC {
      case (Some(a1), Some(a2)) => implicitly[Equal[A]].equal(a1)(a2)
      case (a1, a2) => a1.isDefined == a2.isDefined
    }


  implicit def EitherLeftEqual[A: Equal, B]: Equal[Either.LeftProjection[A, B]] =
    equal(a => b =>
      (a.toOption, b.toOption) match {
        case (Some(a1), Some(a2)) => implicitly[Equal[A]].equal(a1)(a2)
        case (a1, a2) => a1.isDefined == a2.isDefined
      })

  implicit def EitherRightEqual[A, B: Equal]: Equal[Either.RightProjection[A, B]] =
    equal(a => b =>
      (a.toOption, b.toOption) match {
        case (Some(a1), Some(a2)) => implicitly[Equal[B]].equal(a1)(a2)
        case (a1, a2) => a1.isDefined == a2.isDefined
      })

  implicit def EitherEqual[A: Equal, B: Equal]: Equal[Either[A, B]] =
    equalC {
      case (Left(a1), Left(a2)) => implicitly[Equal[A]].equal(a1)(a2)
      case (Right(b1), Right(b2)) => implicitly[Equal[B]].equal(b1)(b2)
      case _ => false
    }

  implicit def MapEqual[CC[K, V] <: collection.Map[K, V], A: Equal, B: Equal]: Equal[CC[A, B]] =
    equalBy(_.toSet)

  import java.{lang => jl, util => ju}

  implicit def JavaIterableEqual[CC[X] <: jl.Iterable[X], A: Equal]: Equal[CC[A]] =
    equalC((a1, a2) => {
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = false

      while (i1.hasNext && i2.hasNext && !b) {
        val x1 = i1.next
        val x2 = i2.next

        if (implicitly[Equal[A]].unequal(x1)(x2)) {
          b = true
        }
      }

      !(b || i1.hasNext || i2.hasNext)
    })

  implicit def JavaMapEntryEqual[K: Equal, V: Equal]: Equal[java.util.Map.Entry[K, V]] =
    equalC((a1, a2) => implicitly[Equal[K]].equal(a1.getKey)(a2.getKey))

  implicit def JavaMapEqual[K: Equal, V: Equal]: Equal[ju.Map[K, V]] =
    equalBy(_.entrySet)

  implicit def CallableEqual[A: Equal]: Equal[java.util.concurrent.Callable[A]] =
    equalBy(_.call)

  implicit def TraversableEqual[CC[X] <: collection.TraversableLike[X, CC[X]] with Traversable[X] : CanBuildAnySelf, A: Equal]: Equal[CC[A]] = equalC(
    (a, b) => {
      implicitly[Equal[A]] match {
        case eq: NaturalEqual =>
          // Performance optimisation.
          a == b
        case _ =>
          // This is an inefficient, but general, way to implement equality with respect to the element equality for an unknown
          // collection type CC with respect to a non-natural equality.
          implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[A, A]
          case class EqualWrap[A: Equal](a: A) {
            override def equals(other: Any) = other match {
              case EqualWrap(b) => implicitly[Equal[A]].equal(a)(b.asInstanceOf[A])
              case _            => false
            }
          }
          a.map(new EqualWrap(_)) == b.map(new EqualWrap(_))
      }
    })

}
