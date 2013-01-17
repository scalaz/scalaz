package scalaz

import std.AllInstances._
import Dual._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class OrderTest extends Spec {
  "duals" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
      val xsdual: List[Int @@ Tags.Dual] = Tag subst xs
      (F maximum xs: Option[Int]) must be_===(F minimum xsdual: Option[Int])
      (F minimum xs: Option[Int]) must be_===(F maximum xsdual: Option[Int])
  }
}
