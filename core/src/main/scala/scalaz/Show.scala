package scalaz

sealed trait Show[A] {
  val show: A => List[Char]
  val shows: A => String

  def text: A => xml.Text =
    a => xml.Text(shows(a))

  def contramap[B](f: B => A): Show[B] =
    Show.show(b => Show.this.show(f(b)))
}

object Show extends Shows

trait Shows {
  def show[A](f: A => List[Char]): Show[A] = new Show[A] {
    val show = f
    val shows = (a: A) => f(a).mkString
  }

  def shows[A](f: A => String): Show[A] = new Show[A] {
    val show = (a: A) => f(a).toList
    val shows = f
  }

  def showA[A]: Show[A] =
    shows[A](_.toString)

  def showBy[A, B: Show](f: A => B): Show[A] =
    implicitly[Show[B]] contramap f

  import Predef.{implicitly => i}

  implicit def ShowContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](f: B => A) =
      r => show[B](b => r show (f(b)))
  }


  implicit def ThrowableShow: Show[Throwable] = showA

  implicit def StringShow: Show[String] = showA

  implicit def SymbolShow: Show[Symbol] = showA

  implicit def UnitShow: Show[Unit] = showA

  implicit def BooleanShow: Show[Boolean] = showA

  implicit def ByteShow: Show[Byte] = showA

  implicit def CharShow: Show[Char] = showA

  implicit def IntShow: Show[Int] = showA

  implicit def LongShow: Show[Long] = showA

  implicit def ShortShow: Show[Short] = showA

  implicit def FloatShow: Show[Float] = showA

  implicit def DoubleShow: Show[Double] = showA

  def UnpackShow[T, R](implicit s: Show[R], u: ^*^[T, R]): Show[T] =
    implicitly[Show[R]] contramap (u.unpack(_))

  implicit def BigIntegerShow: Show[java.math.BigInteger] = showA[java.math.BigInteger]

  implicit def BigIntShow: Show[BigInt] = showA

  implicit def NodeSeqShow: Show[xml.NodeSeq] = showA

  implicit def Function1Show[A, B]: Show[A => B] =
    show((f: A => B) => "<function>".toList)

  implicit def IterableShow[CC[X] <: Iterable[X], A: Show]: Show[CC[A]] = show((as: CC[A]) => {
    val i = as.iterator
    val k = new collection.mutable.ListBuffer[Char]
    k += '['
    while (i.hasNext) {
      val n = i.next
      k ++= implicitly[Show[A]].show(n)
      if (i.hasNext)
        k += ','
    }
    k += ']'
    k.toList
  })

  implicit def Function0Show[A: Show]: Show[Function0[A]] = show(a => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= implicitly[Show[A]].show(a.apply)
    k += ')'
    k.toList
  })

  implicit def Tuple1Show[A: Show]: Show[Tuple1[A]] = show(a => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= implicitly[Show[A]].show(a._1)
    k += ')'
    k.toList
  })

  implicit def Tuple2Show[A: Show, B: Show]: Show[(A, B)] = show {
    case (a, b) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= implicitly[Show[A]].show(a)
      k ++= ", ".toList
      k ++= implicitly[Show[B]].show(b)
      k += ')'
      k.toList
    }
  }

  implicit def Tuple3Show[A: Show, B: Show, C: Show]: Show[(A, B, C)] = show {
    case (a, b, c) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= implicitly[Show[A]].show(a)
      k ++= ", ".toList
      k ++= implicitly[Show[B]].show(b)
      k ++= ", ".toList
      k ++= implicitly[Show[C]].show(c)
      k += ')'
      k.toList
    }
  }

  implicit def Tuple4Show[A: Show, B: Show, C: Show, D: Show]: Show[(A, B, C, D)] = show {
    case (a, b, c, d) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= implicitly[Show[A]].show(a)
      k ++= ", ".toList
      k ++= implicitly[Show[B]].show(b)
      k ++= ", ".toList
      k ++= implicitly[Show[C]].show(c)
      k ++= ", ".toList
      k ++= implicitly[Show[D]].show(d)
      k += ')'
      k.toList
    }
  }

  implicit def Tuple5Show[A: Show, B: Show, C: Show, D: Show, E: Show]: Show[(A, B, C, D, E)] = show {
    case (a, b, c, d, e) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= implicitly[Show[A]].show(a)
      k ++= ", ".toList
      k ++= implicitly[Show[B]].show(b)
      k ++= ", ".toList
      k ++= implicitly[Show[C]].show(c)
      k ++= ", ".toList
      k ++= implicitly[Show[D]].show(d)
      k ++= ", ".toList
      k ++= implicitly[Show[E]].show(e)
      k += ')'
      k.toList
    }
  }

  implicit def Tuple6Show[A: Show, B: Show, C: Show, D: Show, E: Show, F: Show]: Show[(A, B, C, D, E, F)] = show {
    case (a, b, c, d, e, f) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= implicitly[Show[A]].show(a)
      k ++= ", ".toList
      k ++= implicitly[Show[B]].show(b)
      k ++= ", ".toList
      k ++= implicitly[Show[C]].show(c)
      k ++= ", ".toList
      k ++= implicitly[Show[D]].show(d)
      k ++= ", ".toList
      k ++= implicitly[Show[E]].show(e)
      k ++= ", ".toList
      k ++= implicitly[Show[F]].show(f)
      k += ')'
      k.toList
    }
  }

  implicit def Tuple7Show[A: Show, B: Show, C: Show, D: Show, E: Show, F: Show, G: Show]: Show[(A, B, C, D, E, F, G)] = show {
    case (a, b, c, d, e, f, g) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= implicitly[Show[A]].show(a)
      k ++= ", ".toList
      k ++= implicitly[Show[B]].show(b)
      k ++= ", ".toList
      k ++= implicitly[Show[C]].show(c)
      k ++= ", ".toList
      k ++= implicitly[Show[D]].show(d)
      k ++= ", ".toList
      k ++= implicitly[Show[E]].show(e)
      k ++= ", ".toList
      k ++= implicitly[Show[F]].show(f)
      k ++= ", ".toList
      k ++= implicitly[Show[G]].show(g)
      k += ')'
      k.toList
    }
  }

  implicit def OptionShow[A: Show]: Show[Option[A]] = shows(_ map (implicitly[Show[A]].shows(_)) toString)

  implicit def EitherShow[A: Show, B: Show]: Show[Either[A, B]] =
    shows(_.fold(
      implicitly[Show[A]].shows(_)
      , implicitly[Show[B]].shows(_)
    ).toString)

  implicit def MapShow[CC[K, V] <: collection.Map[K, V], A: Show, B: Show]: Show[CC[A, B]] = i[Show[Iterable[(A, B)]]] contramap (z => z)

  import java.{lang => jl, util => ju}

  implicit def JavaIterableShow[CC[X] <: jl.Iterable[X], A: Show]: Show[CC[A]] = {
    import scala.collection.JavaConversions._
    showBy((i: jl.Iterable[A]) => i: Iterable[A])
  }

  implicit def JavaMapEntryShow[K: Show, V: Show]: Show[java.util.Map.Entry[K, V]] =
    show(m => {
      val z = new collection.mutable.ListBuffer[Char]
      z += '('
      z ++= implicitly[Show[K]].shows(m.getKey)
      z += ','
      z ++= implicitly[Show[V]].shows(m.getValue)
      z += ')'
      z.toList
    })

  implicit def JavaMapShow[K: Show, V: Show]: Show[ju.Map[K, V]] = show(m => {
    val z = new collection.mutable.ListBuffer[Char]
    z += '{'
    val i = m.keySet.iterator
    while (i.hasNext) {
      val k = i.next
      val v = m get k
      z ++= implicitly[Show[K]].show(k)
      z ++= " -> ".toList
      z ++= implicitly[Show[V]].show(v)
      if (i.hasNext)
        z += ','
    }
    z += '}'
    z.toList
  })

  implicit def CallableShow[A: Show]: Show[java.util.concurrent.Callable[A]] =
    showBy(_.call)

  implicit def IdentityShow[A: Show]: Show[Identity[A]] =
    Show.showBy(_.value)

  implicit def DigitShow: Show[Digit] =
    Show.showBy(_.toInt)

  implicit def NonEmptyListShow[A: Show]: Show[NonEmptyList[A]] =
    implicitly[Show[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

  implicit def ShowOptionT[F[_], A](implicit e: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    Show.showBy(_.runT)

  implicit def TreeShow[A: Show]: Show[Tree[A]] =
    Show.show((t: Tree[A]) =>
      '{' :: implicitly[Show[A]].show(t.rootLabel) ++ " " ++ implicitly[Show[Stream[Tree[A]]]].show(t.subForest) ++ "}")

}