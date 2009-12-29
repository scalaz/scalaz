package scalaz

trait Show[-A] {
  def show(a: A): List[Char]
}

trait Shows {
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }

  def shows[A](f: A => String): Show[A] = show[A](f(_).toList)

  def showA[A] = shows[A](_.toString)
}

object Show {
  import Scalaz._
  import Predef.{implicitly => i}

  implicit def DigitShow: Show[Digit] = i[Show[Int]] ∙ ((_: Digit).toInt)

  implicit def OrderingShow: Show[Ordering] = showA

  implicit def UnitShow: Show[Unit] = showA

  implicit def ThrowableShow: Show[Throwable] = showA

  implicit def StringShow: Show[String] = showA

  implicit def IntShow: Show[Int] = showA

  implicit def IntMultiplicationShow: Show[IntMultiplication] = i[Show[Int]] ∙ ((_: IntMultiplication).value)

  implicit def BooleanShow: Show[Boolean] = showA

  implicit def BooleanConjunctionShow: Show[BooleanConjunction] = i[Show[Boolean]] ∙ ((_: BooleanConjunction).value)

  implicit def CharShow: Show[Char] = showA

  implicit def CharMultiplicationShow: Show[CharMultiplication] = i[Show[Char]] ∙ ((_: CharMultiplication).value)

  implicit def ByteShow: Show[Byte] = showA

  implicit def ByteMultiplicationShow: Show[ByteMultiplication] = i[Show[Byte]] ∙ ((_: ByteMultiplication).value)

  implicit def LongShow: Show[Long] = showA

  implicit def LongMultiplicationShow: Show[LongMultiplication] = i[Show[Long]] ∙ ((_: LongMultiplication).value)

  implicit def ShortShow: Show[Short] = showA

  implicit def ShortMultiplicationShow: Show[ShortMultiplication] = i[Show[Short]] ∙ ((_: ShortMultiplication).value)

  implicit def FloatShow: Show[Float] = showA

  implicit def DoubleShow: Show[Double] = showA

  implicit def BigIntegerShow: Show[java.math.BigInteger] = showA[java.math.BigInteger]

  implicit def BigIntegerMultiplicationShow: Show[BigIntegerMultiplication] = BigIntegerShow ∙ ((_: BigIntegerMultiplication).value)

  implicit def BigIntShow: Show[BigInt] = showA

  implicit def BigIntMultiplicationShow: Show[BigIntMultiplication] = BigIntShow ∙ ((_: BigIntMultiplication).value)

  implicit def NodeSeqShow: Show[xml.NodeSeq] = showA

  implicit def NonEmptyListShow[A: Show]: Show[NonEmptyList[A]] = IterableShow[A] ∙ ((_: NonEmptyList[A]).list)

  implicit def ZipStreamShow[A: Show]: Show[ZipStream[A]] = IterableShow[A] ∙ ((_: ZipStream[A]).value)

  implicit def ZipperShow[A: Show]: Show[Zipper[A]] = show((z: Zipper[A]) =>
      z.lefts.reverse.show ++ " " ++ z.focus.show ++ " " ++ z.rights.show)

  implicit def TreeShow[A: Show]: Show[Tree[A]] = show((t: Tree[A]) =>
      '{' :: t.rootLabel.show ++ " " ++ t.subForest.show ++ "}")

  implicit def TreeLocShow[A: Show]: Show[TreeLoc[A]] = show((t: TreeLoc[A]) =>
      t.toTree.show ++ "@" ++ t.parents.map(_._1.length).reverse.show)

  def IterableShow[A: Show]: Show[Iterable[A]] = show(as => {
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

  implicit def StreamShow[A: Show]: Show[Stream[A]] = IterableShow

  implicit def GArrayShow[A: Show]: Show[GArray[A]] = IterableShow

  implicit def ListShow[A: Show]: Show[List[A]] = IterableShow

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

  implicit def Function0Show[A: Show]: Show[Function0[A]] = show(_.apply.show)

  implicit def OptionShow[A: Show]: Show[Option[A]] = shows(_ map (_.shows) toString)

  implicit def EitherShow[A: Show, B: Show]: Show[Either[A, B]] = shows(e => (((_: A).shows) <-: e :-> (_.shows)).toString)

  implicit def ValidationShow[E: Show, A: Show]: Show[Validation[E, A]] = shows {
    case Success(a) => "Success(" + a.shows + ")"
    case Failure(e) => "Failure(" + e.shows + ")"
  }

  implicit def JavaIterableShow[A: Show]: Show[java.lang.Iterable[A]] = show(as => {
    val k = new collection.mutable.ListBuffer[Char]
    val i = as.iterator
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

  implicit def JavaMapShow[K: Show, V: Show]: Show[java.util.Map[K, V]] = show(m => {
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

  import geo._

  implicit def AzimuthShow: Show[Azimuth] = shows(_.value.shows + "°")

  implicit def BearingShow: Show[Bearing] = shows(_.value.shows + "°")

  implicit def CoordShow: Show[Coord] = shows(c => "[" + c.latitude.shows + " " + c.longitude.shows + "]")

  implicit def ElevatedCurveShow: Show[ElevatedCurve] = i[Show[(GeodeticCurve, Elevation)]] ∙ (((_: ElevatedCurve).curve) &&& ((_: ElevatedCurve).elevation))

  implicit def ElevationShow: Show[Elevation] = shows(_.value.shows + "m")

  implicit def EllipsoidShow: Show[Ellipsoid] = i[Show[(Double, Double, Double, Double)]] ∙ (e => (e.semiMajor, e.semiMinor, e.flattening, e.inverseFlattening))

  implicit def GeodeticCurveShow: Show[GeodeticCurve] = shows(c => "[" + c.ellipsoidalDistance.shows + " " + c.azi.shows + " " + c.reverseAzi.shows + "]")

  implicit def LatitudeShow: Show[Latitude] = shows(_.value.shows + "°")

  implicit def LongitudeShow: Show[Longitude] = shows(_.value.shows + "°")

  implicit def PositionShow: Show[Position] = i[Show[(Coord, Elevation)]] ∙ (((_: Position).coord) &&& ((_: Position).elevation))

  implicit def VectorShow: Show[Vector] = i[Show[(Coord, Bearing)]] ∙ (((_: Vector).coord) &&& ((_: Vector).bearing))
}
