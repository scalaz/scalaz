package scalaz.example

object SyntaxUsage extends App {

  val o1: Option[Int] = Some(0)
  val o2: Option[Option[Int]] = Some(Some(0))
  val l1: List[String] = List("one")
  val l2: List[List[String]] = List(List("one"))

  syntax1()
  syntax2()
  syntax3()
  stdSyntax()

  // Use the syntax only for Monad[Option]
  // This includes the syntax for the parent type classes.
  def syntax1() {
    import scalaz._

    // Import type class instances for Option, and the
    // Monad syntax for Option.
    import std.Option._
    import std.Option.option.monadSyntax._
    val x = 1.pure
    val y = pure(1)
    x: Option[Int]
    y: Option[Int]

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
  }

  // Use two different instances, and the syntax for all Monads
  def syntax2() {
    import scalaz._

    // Import type class instances for Option and List
    import std.Option._
    import std.List._

    // Implicit conversions from M[A] => BindV[M, A], etc.
    import syntax.monad._

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
    l2.join

    1.pure[Option]
  }

  def syntax3() {
    import scalaz._

    // Import all type class instances
    import Scalaz._

    // Import all conversions for syntax for all type classes
    import syntax.all._

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
    l2.join

    o2.pair(o2)
  }


  // Monad extends from Pointed, so we can use (std.Option.option: Monad[Option]) where Pointed[F] is called for.
  def useParentTypeClass {
    import scalaz._
    
    def needPointed[F[_]: Pointed] = ()
    
    import std.Option._
    needPointed[Option]
  }
  
  def stdSyntax() {
    import scalaz.std.AnyVal._
    import scalaz.std.Stream.streamSyntax._

    val merged = Stream(1, 3, 5).merge(Stream(2, 4, 6))

    import scalaz.std.List._
    import scalaz.std.Option._
    import scalaz.std.Option.optionSyntax._
    import scalaz.syntax.monoid._
    import scalaz.syntax.equal._

    val lists: List[Int] = some(1).orEmpty[List]
    ((some(1).last |+| some(2).last): Option[Int]) assert_=== some(2)
  }
}
