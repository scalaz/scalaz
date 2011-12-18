package scalaz

import java.math.BigInteger
import scalacheck.ScalazArbitrary
import scalaz.scalacheck.ScalazProperties.{equal, order}
import scala.math.{Ordering => SOrdering}
import org.scalacheck.{Arbitrary, Properties, Prop}
import Prop.forAll

class OrderTest extends Spec {

  import Scalaz._
  import std.iterable._
  import Tags._
  import ScalazArbitrary._

  type A = String
  type B = String
  type C = String
  type D = String
  type E = String
  type F = String
  type G = String
  type H = String
  type K = String
  type V = String
  type X = String

  def scalaOrdering[A: Order: SOrdering: Arbitrary] = forAll((a1: A, a2: A) => Order[A].order(a1, a2) == Ordering.fromInt(SOrdering[A].compare(a1, a2)))

  checkAll("Unit", order.laws[Unit])
  checkAll("Boolean", order.laws[Boolean].withProp("benchmark", scalaOrdering[Boolean]))
  checkAll("Char", order.laws[Char].withProp("benchmark", scalaOrdering[Char]))
  checkAll("Short", order.laws[Short].withProp("benchmark", scalaOrdering[Short]))
  checkAll("Int", order.laws[Int].withProp("benchmark", scalaOrdering[Int]))
  checkAll("Long", order.laws[Long].withProp("benchmark", scalaOrdering[Long]))
  checkAll("Float", order.laws[Float].withProp("benchmark", scalaOrdering[Float]))

  checkAll("Ordering", order.laws[Ordering])
  checkAll("String", order.laws[String].withProp("benchmark", scalaOrdering[String]))
  checkAll("Int @@ Multiplication", order.laws[Int @@ Multiplication])
  checkAll("Boolean @@ Conjunction", order.laws[Boolean @@ Conjunction])
  checkAll("Char @@ Multiplication", order.laws[Char @@ Multiplication])
  checkAll("Byte @@ Multiplication", order.laws[Byte @@ Multiplication])
  checkAll("Long @@ Multiplication", order.laws[Long @@ Multiplication])
  checkAll("Short @@ Multiplication", order.laws[Short @@ Multiplication])
  checkAll("BigInteger", order.laws[BigInteger])
  checkAll("BigInteger @@ Multiplication", order.laws[BigInteger @@ Multiplication])
  checkAll("BigInt", order.laws[BigInt])
  checkAll("BigInt @@ Multiplication", order.laws[BigInt @@ Multiplication])
  checkAll("(A)", order.laws[(A)])
  checkAll("(A, B)", order.laws[(A, B)])
  checkAll("(A, B, C)", order.laws[(A, B, C)])
  checkAll("(A, B, C, D)", order.laws[(A, B, C, D)])
  checkAll("(A, B, C, D, E)", order.laws[(A, B, C, D, E)])
  checkAll("(A, B, C, D, E, F)", order.laws[(A, B, C, D, E, F)])
  checkAll("(A, B, C, D, E, F, G)", order.laws[(A, B, C, D, E, F, G)])
  checkAll("(A, B, C, D, E, F, G, H)", order.laws[(A, B, C, D, E, F, G, H)])
  checkAll("Option", order.laws[Option[A]].withProp("benchmark", scalaOrdering[Option[A]]))
  checkAll("Option @@ First", order.laws[Option[A] @@ First])
  checkAll("Option @@ Last", order.laws[Option[A] @@ Last])
  checkAll("Either", order.laws[Either[A, B]])
  checkAll("Either.LeftProjection", order.laws[Either.LeftProjection[A, X]])
  checkAll("Either.LeftProjection @@ First", order.laws[Either.LeftProjection[A, X] @@ First])
  checkAll("Either.LeftProjection @@ Last", order.laws[Either.LeftProjection[A, X] @@ Last])
  checkAll("Either.RightProjection", order.laws[Either.RightProjection[X, A]])
  checkAll("Either.RightProjection @@ First", order.laws[Either.RightProjection[X, A] @@ First])
  checkAll("Either.RightProjection @@ Last", order.laws[Either.RightProjection[X, A] @@ Last])
  checkAll("Validation", order.laws[Validation[E, A]])
  checkAll("FailProjection", order.laws[FailProjection[E, A]])

  checkAll("Iterable", order.laws[Iterable[Boolean]].withProp("benchmark", scalaOrdering[Iterable[Boolean]]))
}
