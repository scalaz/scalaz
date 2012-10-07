package scalaz
package typelevel

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Arbitrary

class CompositionTest extends Spec {

  import typelevel.syntax.all._

  checkAll("Identity composition", applicative.laws[TCNil#Composed])
  checkAll("Identity composition", traverse.laws[TCNil#Composed])

  checkAll("Single composition", applicative.laws[TCCons[List, TCNil]#Composed])
  checkAll("Single composition", traverse.laws[TCCons[List, TCNil]#Composed])

  implicit val nestedApplicative = (Applicative[Option] <<: Applicative[List] <<: KTypeClass[Applicative].idCompose).instance
  implicit val nestedTraverse = (Traverse[Option] <<: Traverse[List] <<: KTypeClass[Traverse].idCompose).instance

  checkAll("Double composition", applicative.laws[TCCons[Option, TCCons[List, TCNil]]#Composed])
  checkAll("Double composition", traverse.laws[TCCons[Option, TCCons[List, TCNil]]#Composed])

  implicit val nestedNestedApplicative = (Applicative[List] <<: Applicative[Option] <<: Applicative[List] <<: KTypeClass[Applicative].idCompose).instance
  implicit val nestedNestedTraverse = (Traverse[List] <<: Traverse[Option] <<: Traverse[List] <<: KTypeClass[Traverse].idCompose).instance

  checkAll("Triple composition", applicative.laws[TCCons[List, TCCons[Option, TCCons[List, TCNil]]]#Composed])
  checkAll("Triple composition", traverse.laws[TCCons[List, TCCons[Option, TCCons[List, TCNil]]]#Composed])

}

// vim: expandtab:ts=2:sw=2
