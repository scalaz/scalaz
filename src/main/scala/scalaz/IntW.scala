package scalaz

sealed trait IntW {
  val value: Int

  import S._
  
  def |*| = IntMultiplication.multiplication(value)

  def ordering = if(value < 0) LT else if(value > 0) GT else EQ

  import test._

  def >-->(high: Int): Gen[Int] = if(value > high) Gen.fail else Gen.randomised(_.chooseInt(value, high).gen)

  def <=-[A](a: A*): Gen[List[A]] = {
    val l = a.length
    if(value < 0 || value > l)
      Gen.fail
    else Gen.gen((sz, r) => {
      val k = new collection.mutable.ListBuffer[A]
      k ++= a
      while(k.length > value) k.remove((0 >--> k.length - 1)(sz)(r))     
      k.toList
    })
  }
}

object IntW {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }

  implicit def IntFrom(n: IntW) = n.value
}
