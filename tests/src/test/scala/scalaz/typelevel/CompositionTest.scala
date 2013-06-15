package scalaz
package typelevel

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class CompositionTest extends Spec {


  checkAll("Identity composition", applicative.laws[TCNil#Composed])
  checkAll("Identity composition", traverse.laws[TCNil#Composed])

  checkAll("Single composition", applicative.laws[TCCons[List, TCNil]#Composed])
  checkAll("Single composition", traverse.laws[TCCons[List, TCNil]#Composed])

  implicit val nestedApplicative = (Applicative[Option] <<: Applicative[List] <<: KTypeClass[Applicative].idCompose).instance
  implicit val nestedTraverse = (Traverse[Option] <<: Traverse[List] <<: KTypeClass[Traverse].idCompose).instance

  checkAll("Double composition", applicative.laws[TCCons[Option, TCCons[List, TCNil]]#Composed])
  checkAll("Double composition", traverse.laws[TCCons[Option, TCCons[List, TCNil]]#Composed])

}

// vim: expandtab:ts=2:sw=2
