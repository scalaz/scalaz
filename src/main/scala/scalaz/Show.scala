package scalaz

trait Show[-A] {
  def show(a: A): List[Char]

  def shows(a: A) = show(a).mkString
}

object Show {
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }

  def shows[A](f: A => String) = show[A](f(_).toList)

  def showA[A] = shows[A](_.toString)

  import S._
  import MA._
  import MAB._

  implicit val DigitShow = IntShow <| ((_: Digit).toInt)

  implicit val OrderingShow = showA[Ordering]

  implicit val UnitShow = showA[Unit]

  implicit val StringShow = showA[String]

  implicit val IntShow = showA[Int]

  implicit val IntMultiplicationShow = IntShow <| ((_: IntMultiplication).value)

  implicit val BooleanShow = showA[Boolean]

  implicit val BooleanConjunctionShow = BooleanShow <| ((_: BooleanConjunction).value)

  implicit val CharShow = showA[Char]

  implicit val CharMultiplicationShow = CharShow <| ((_: CharMultiplication).value)

  implicit val ByteShow = showA[Byte]

  implicit val ByteMultiplicationShow = ByteShow <| ((_: ByteMultiplication).value)

  implicit val LongShow = showA[Long]

  implicit val LongMultiplicationShow = LongShow <| ((_: LongMultiplication).value)

  implicit val ShortShow = showA[Short]

  implicit val ShortMultiplicationShow = ShortShow <| ((_: ShortMultiplication).value)

  implicit val FloatShow = showA[Float]

  implicit val DoubleShow = showA[Double]

  implicit val BigIntegerShow = showA[java.math.BigInteger]

  implicit val BigIntegerMultiplicationShow = BigIntegerShow <| ((_: BigIntegerMultiplication).value)

  implicit val BigIntShow = showA[BigInt]

  implicit val BigIntMultiplicationShow = BigIntShow <| ((_: BigIntMultiplication).value)

  implicit val NodeSeqShow = showA[xml.NodeSeq]

  implicit def NonEmptyListShow[A](implicit sa: Show[A]): Show[NonEmptyList[A]] = IterableShow(sa) <| ((_: NonEmptyList[A]).list)

  implicit def ZipStreamShow[A](implicit sa: Show[A]): Show[ZipStream[A]] = IterableShow(sa) <| ((_: ZipStream[A]).value)

  implicit def ZipperShow[A](implicit sa: Show[A]): Show[Zipper[A]] = show((z: Zipper[A]) =>
      z.lefts.reverse.show ++ " " ++ sa.show(z.focus) ++ " " ++ z.rights.show)

  implicit def IterableShow[A](implicit sa: Show[A]) = show[Iterable[A]](as => {
    val i = as.elements
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

  implicit def Tuple1Show[A](implicit sa: Show[A]) = show[Tuple1[A]](a => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= a._1.show
    k += ')'
    k.toList
  })

  implicit def Tuple2Show[A, B](implicit sa: Show[A], sb: Show[B]) = show[Tuple2[A, B]]{ case (a, b) => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= a.show
    k ++= ", ".toList
    k ++= b.show
    k += ')'
    k.toList
  }}

  implicit def Tuple3Show[A, B, C](implicit sa: Show[A], sb: Show[B], sc: Show[C]) = show[Tuple3[A, B, C]]{ case (a, b, c) => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= a.show
    k ++= ", ".toList
    k ++= b.show
    k ++= ", ".toList
    k ++= c.show
    k += ')'
    k.toList
  }}

  implicit def Tuple4Show[A, B, C, D](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D]) = show[Tuple4[A, B, C, D]]{ case (a, b, c, d) => {
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
  }}

  implicit def Tuple5Show[A, B, C, D, E](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D], se: Show[E]) = show[Tuple5[A, B, C, D, E]]{ case (a, b, c, d, e) => {
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
  }}

  implicit def Tuple6Show[A, B, C, D, E, F](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D], se: Show[E], sf: Show[F]) = show[Tuple6[A, B, C, D, E, F]]{ case (a, b, c, d, e, f) => {
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
  }}

  implicit def Tuple7Show[A, B, C, D, E, F, G](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D], se: Show[E], sf: Show[F], sg: Show[G]) = show[Tuple7[A, B, C, D, E, F, G]]{ case (a, b, c, d, e, f, g) => {
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
  }}

  implicit def Function0Show[A](implicit sa: Show[A]) = show[Function0[A]](_.apply.show)  

  implicit def OptionShow[A](implicit sa: Show[A]) = shows[Option[A]](_ map (_.shows) toString)

  implicit def EitherShow[A, B](implicit sa: Show[A], sb: Show[B]) = shows[Either[A, B]](e => (((_: A).shows) <-: e :-> (_.shows)).toString)

  implicit def JavaIterableShow[A](implicit sa: Show[A]) = show[java.lang.Iterable[A]](as => {
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

  implicit def JavaMapShow[K, V](implicit sk: Show[K], sv: Show[V]) = show[java.util.Map[K, V]](m => {
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
