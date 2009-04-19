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
    while(i.hasNext) {
      val n = i.next
      k ++= n.show
      if(i.hasNext)
        k += ','
    }
    k.toList
  })

  implicit def OptionShow[A](implicit sa: Show[A]) = shows[Option[A]](_ map (_.shows) toString)

}
