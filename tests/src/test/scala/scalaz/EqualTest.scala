package scalaz

import java.math.BigInteger
import scalacheck.{ScalazArbitrary}
import scalaz.scalacheck.ScalazProperties.equal

class EqualTest extends Spec {

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

  // AnyVal
  checkAll("Unit", equal.laws[Unit]) 
  checkAll("Boolean", equal.laws[Boolean]) 
  checkAll("Char", equal.laws[Char]) 
  checkAll("Short", equal.laws[Short]) 
  checkAll("Int", equal.laws[Int]) 
  checkAll("Long", equal.laws[Long]) 
  checkAll("Float", equal.laws[Float]) 

  //    checkAll("Digit", equal.laws[Digit]) 
  checkAll("Ordering", equal.laws[Ordering]) 
  checkAll("String", equal.laws[String]) 
  checkAll("Int @@ Multiplication", equal.laws[Int @@ Multiplication]) 
  checkAll("Boolean @@ Conjunction", equal.laws[Boolean @@ Conjunction]) 
  checkAll("Char @@ Multiplication", equal.laws[Char @@ Multiplication]) 
  checkAll("Byte @@ Multiplication", equal.laws[Byte @@ Multiplication]) 
  checkAll("Long @@ Multiplication", equal.laws[Long @@ Multiplication]) 
  checkAll("Short @@ Multiplication", equal.laws[Short @@ Multiplication]) 
  checkAll("BigInteger", equal.laws[BigInteger]) 
  checkAll("BigInteger @@ Multiplication", equal.laws[BigInteger @@ Multiplication]) 
  checkAll("BigInt", equal.laws[BigInt]) 
  checkAll("BigInt @@ Multiplication", equal.laws[BigInt @@ Multiplication]) 
  // todo Arbitrary instance
  // checkAll("xml.NodeSeq", equal.laws[xml.NodeSeq]) 
  checkAll("NonEmptyList", equal.laws[NonEmptyList[A]])

  //    checkAll("ZipStream[A]", equal.laws[ZipStream[A]]) 
  checkAll("(A)", equal.laws[(A)]) 
  checkAll("(A, B)", equal.laws[(A, B)]) 
  checkAll("(A, B, C)", equal.laws[(A, B, C)]) 
  checkAll("(A, B, C, D)", equal.laws[(A, B, C, D)]) 
  checkAll("(A, B, C, D, E)", equal.laws[(A, B, C, D, E)]) 
  checkAll("(A, B, C, D, E, F)", equal.laws[(A, B, C, D, E, F)]) 
  checkAll("(A, B, C, D, E, F, G)", equal.laws[(A, B, C, D, E, F, G)]) 
  checkAll("(A, B, C, D, E, F, G, H)", equal.laws[(A, B, C, D, E, F, G, H)]) 
  checkAll("() => A", equal.laws[() => A]) 
  checkAll("Option", equal.laws[Option[A]])
  checkAll("Option @@ First", equal.laws[Option[A] @@ First])
  checkAll("Option @@ Last", equal.laws[Option[A] @@ Last])
  checkAll("Either", equal.laws[Either[A, B]])
  checkAll("Either.LeftProjection", equal.laws[Either.LeftProjection[A, X]])
  checkAll("Either.LeftProjection @@ First", equal.laws[Either.LeftProjection[A, X] @@ First])
  checkAll("Either.LeftProjection @@ Last", equal.laws[Either.LeftProjection[A, X] @@ Last])
  checkAll("Either.RightProjection", equal.laws[Either.RightProjection[X, A]])
  checkAll("Either.RightProjection @@ First", equal.laws[Either.RightProjection[X, A] @@ First])
  checkAll("Either.RightProjection @@ Last", equal.laws[Either.RightProjection[X, A] @@ Last])
  checkAll("Validation", equal.laws[Validation[E, A]])
  checkAll("FailProjection", equal.laws[FailProjection[E, A]])
  // todo Arbitrary for Tree producing large (infinite?) trees.
  //    checkAll("Tree[A]", equal.laws[Tree[A]]) 
  //    checkAll("TreeLoc[A]", equal.laws[TreeLoc[A]]) 

  // todo add Arbitrary instance
  //checkAll("Promise[A]", equal.laws[Promise[A]]) 
  checkAll("List[A]", equal.laws[List[A]]) 
  checkAll("Stream[A]", equal.laws[Stream[A]]) 
  checkAll("Iterable[A]", equal.laws[Iterable[A]]) 
  //    checkAll("ArraySeq[A]", equal.laws[ArraySeq[A]]) 

  //checkAll("java.util.concurrent.Callable[A]", equal.laws[java.util.concurrent.Callable[A]])
  checkAll("Zipper[A]", equal.laws[Zipper[A]]) 
}
