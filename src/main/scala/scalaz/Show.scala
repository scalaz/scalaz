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

  implicit val IntShow = showA[Int]

  implicit val BooleanShow = showA[Boolean]

  implicit val CharShow = showA[Char]

  implicit val ByteShow = showA[Byte]

  implicit val LongShow = showA[Long]

  implicit val ShortShow = showA[Short]

  implicit val FloatShow = showA[Float]

  implicit val DoubleShow = showA[Double]

  implicit val StringShow = showA[String]

  import S._
  
  implicit def IterableShow[A](implicit sa: Show[A]) = show[Iterable[A]](as => {
    val i = as.elements
    val k = new collection.mutable.ListBuffer[Char]
    k += '['
    while(i.hasNext) {
      val n = i.next
      k ++= n.show
      if(i.hasNext)
        k += ','
    }
    k += ']'
    k.toList
  })

  implicit def OptionShow[A](implicit sa: Show[A]) = shows[Option[A]](_ map (_.shows) toString)

  import MAB._

  implicit def EitherShow[A, B](implicit sa: Show[A], sb: Show[B]) = shows[Either[A, B]](e => (((_: A).shows) <-: e :-> (_.shows)).toString)

  implicit def JavaIterableShow[A](implicit sa: Show[A]) = show[java.lang.Iterable[A]](as => {
    val k = new collection.mutable.ListBuffer[Char]
    val i = as.iterator
    k += '['
    while(i.hasNext) {
      val n = i.next
      k ++= n.show
      if(i.hasNext)
        k += ','
    }
    k += ']'
    k.toList    
  })

  implicit def JavaMapShow[K, V](implicit sk: Show[K], sv: Show[V]) = show[java.util.Map[K, V]](m => {
    val z = new collection.mutable.ListBuffer[Char]
    z += '{'
    val i = m.keySet.iterator
    while(i.hasNext) {
      val k = i.next
      val v = m get k
      z ++= k.show
      z ++= " -> ".toList
      z ++= v.show
      if(i.hasNext)
        z += ','
    }
    z += '}'
    z.toList
  })
}
