package scalaz

trait Show[A] {
  def show(a: A): List[Char]
}

trait Shows {
  import Scalaz._
  def show[A](f: A => List[Char]): Show[A] = new Show[A] {
    def show(a: A) = f(a)
  }

  def shows[A](f: A => String): Show[A] = show[A](f(_).toList)

  def showA[A]: Show[A] = shows[A](_.toString)
  
  def showBy[A, B: Show](f: A => B): Show[A] = implicitly[Show[B]] ∙ f
}

object Show {
  import Scalaz._
  import Predef.{implicitly => i}

  implicit def DigitShow: Show[Digit] = showBy(_.toInt)

  implicit def OrderingShow: Show[Ordering] = showA

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

  def NewTypeShow[B: Show, A <: NewType[B]]: Show[A] = showBy(_.value)

  implicit def IntMultiplicationShow: Show[IntMultiplication] = NewTypeShow[Int, IntMultiplication]

  implicit def BooleanConjunctionShow: Show[BooleanConjunction] = NewTypeShow[Boolean, BooleanConjunction]

  implicit def CharMultiplicationShow: Show[CharMultiplication] = NewTypeShow[Char, CharMultiplication]

  implicit def ByteMultiplicationShow: Show[ByteMultiplication] = NewTypeShow[Byte, ByteMultiplication]

  implicit def LongMultiplicationShow: Show[LongMultiplication] = NewTypeShow[Long, LongMultiplication]

  implicit def ShortMultiplicationShow: Show[ShortMultiplication] = NewTypeShow[Short, ShortMultiplication]

  implicit def BigIntegerShow: Show[java.math.BigInteger] = showA[java.math.BigInteger]

  implicit def BigIntegerMultiplicationShow: Show[BigIntegerMultiplication] = NewTypeShow[java.math.BigInteger, BigIntegerMultiplication]

  implicit def BigIntShow: Show[BigInt] = showA

  implicit def BigIntMultiplicationShow: Show[BigIntMultiplication] = NewTypeShow[BigInt, BigIntMultiplication]

  implicit def ConstShow[B: Show, A]: Show[Const[B, A]] = NewTypeShow[B, Const[B, A]]

  implicit def NodeSeqShow: Show[xml.NodeSeq] = showA

  implicit def NonEmptyListShow[A: Show]: Show[NonEmptyList[A]] = implicitly[Show[Iterable[A]]] ∙ ((_: NonEmptyList[A]).list)

  implicit def Function1Show[A, B]: Show[A => B] =
    show((f: A => B) => "<function>".toList)

  implicit def ZipStreamShow[A: Show]: Show[ZipStream[A]] = implicitly[Show[Stream[A]]] ∙ ((_: ZipStream[A]).value)

  implicit def ZipperShow[A: Show]: Show[Zipper[A]] = show((z: Zipper[A]) =>
    z.lefts.reverse.show ++ " " ++ z.focus.show ++ " " ++ z.rights.show)

  implicit def TreeShow[A: Show]: Show[Tree[A]] = show((t: Tree[A]) =>
    '{' :: t.rootLabel.show ++ " " ++ t.subForest.show ++ "}")

  implicit def TreeLocShow[A: Show]: Show[TreeLoc[A]] = show((t: TreeLoc[A]) =>
    t.toTree.show ++ "@" ++ t.parents.map(_._1.length).reverse.show)

  implicit def IterableShow[CC[X] <: Iterable[X], A: Show]: Show[CC[A]] = show((as: CC[A]) => {
    val i = as.iterator
    val k = new collection.mutable.ListBuffer[Char]
    k += '['
    while (i.hasNext) {
      val n = i.next
      k ++= n.show
      if (i.hasNext)
        k += ','
    }
    k += ']'
    k.toList
  })

  implicit def Tuple1Show[A: Show]: Show[Tuple1[A]] = show(a => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= a._1.show
    k += ')'
    k.toList
  })

  implicit def Tuple2Show[A: Show, B: Show]: Show[(A, B)] = show {
    case (a, b) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple3Show[A: Show, B: Show, C: Show]: Show[(A, B, C)] = show {
    case (a, b, c) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple4Show[A: Show, B: Show, C: Show, D: Show]: Show[(A, B, C, D)] = show {
    case (a, b, c, d) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple5Show[A: Show, B: Show, C: Show, D: Show, E: Show]: Show[(A, B, C, D, E)] = show {
    case (a, b, c, d, e) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k ++= ", ".toList
      k ++= e.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple6Show[A: Show, B: Show, C: Show, D: Show, E: Show, F: Show]: Show[(A, B, C, D, E, F)] = show {
    case (a, b, c, d, e, f) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k ++= ", ".toList
      k ++= e.show
      k ++= ", ".toList
      k ++= f.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple7Show[A: Show, B: Show, C: Show, D: Show, E: Show, F: Show, G: Show]: Show[(A, B, C, D, E, F, G)] = show {
    case (a, b, c, d, e, f, g) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k ++= ", ".toList
      k ++= e.show
      k ++= ", ".toList
      k ++= f.show
      k ++= ", ".toList
      k ++= g.show
      k += ')'
      k.toList
    }
  }

  implicit def Function0Show[A: Show]: Show[() => A] = show(_.apply.show)

  implicit def OptionShow[A: Show]: Show[Option[A]] = shows(_ map (_.shows) toString)

  implicit def FirstOptionShow[A: Show]: Show[FirstOption[A]] = OptionShow[A] ∙ ((_: FirstOption[A]).value)

  implicit def LastOptionShow[A: Show]: Show[LastOption[A]] = OptionShow[A] ∙ ((_: LastOption[A]).value)

  implicit def EitherShow[A: Show, B: Show]: Show[Either[A, B]] = shows(e => (((_: A).shows) <-: e :-> (_.shows)).toString)

  implicit def ValidationShow[E: Show, A: Show]: Show[Validation[E, A]] = shows {
    case Success(a) => "Success(" + a.shows + ")"
    case Failure(e) => "Failure(" + e.shows + ")"
  }

  implicit def MapShow[CC[K, V] <: collection.Map[K, V], A: Show, B: Show]: Show[CC[A, B]] = i[Show[Iterable[(A, B)]]] covary

  import java.{lang => jl, util => ju}

  implicit def JavaIterableEqual[CC[X] <: jl.Iterable[X], A: Show]: Show[CC[A]] = {
    import scala.collection.JavaConversions._
    showBy((i: jl.Iterable[A]) => i: Iterable[A])
  }

  implicit def JavaMapShow[K: Show, V: Show]: Show[ju.Map[K, V]] = show(m => {
    import collection.JavaConversions

    val z = new collection.mutable.ListBuffer[Char]
    z += '{'
    val i = m.keySet.iterator
    while (i.hasNext) {
      val k = i.next
      val v = m get k
      z ++= k.show
      z ++= " -> ".toList
      z ++= v.show
      if (i.hasNext)
        z += ','
    }
    z += '}'
    z.toList
  })
}
