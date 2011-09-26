package scalaz.example

import scalaz.instance

object SyntaxUsage extends App {

  val o1: Option[Int] = Some(0)
  val o2: Option[Option[Int]] = Some(Some(0))
  val l1: List[String] = List("one")
  val l2: List[List[String]] = List(List("one"))

  syntax1()
  syntax2()

  def syntax1() {
    import scalaz._

    // Import type class instances for Option and List
    import instance.Option._
    import instance.List._

    // Implicit conversions from M[A] => BindV[M, A], etc.
    import syntax.Syntax.monad._

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
    l2.join
  }

  def syntax2() {
    import scalaz._

    // Import all type class instances
    import Scalaz._

    // Import all conversions for syntax for all type classes
    import syntax.Syntax.all._

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
    l2.join
  }

  def useParentTypeClass {
    import scalaz._
    
    def needPointed[F[_]: Pointed] = ()
    
    import instance.Option._
    needPointed[Option]
  }
}
