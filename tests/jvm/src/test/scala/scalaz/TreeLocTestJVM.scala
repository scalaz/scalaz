package scalaz

import scalaz.scalacheck.ScalazArbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object TreeLocTestJVM extends SpecLite {

  "ScalazArbitrary.treeLocGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = ScalazArbitrary.treeLocGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[TreeLoc].length(_)).forall(_ == size)
  }

}
