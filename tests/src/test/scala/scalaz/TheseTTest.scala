package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll


object TheseTTest extends SpecLite {
  type TheseTList[A, B] = TheseT[List, A, B]
  type TheseTListInt[A] = TheseT[List, Int, A]
  type TheseTOptionInt[A] = TheseT[Option, Int, A]

  implicit def TheseTListIntEqual[A: Equal]: Equal[TheseTListInt[A]] = new Equal[TheseTListInt[A]] {
    ////
    import scalaz.syntax.equal._
    import scalaz.std.list._
    override def equal(a1: TheseTListInt[A], a2: TheseTListInt[A]) =
      a1.run === a2.run
  }

  checkAll(monad.laws[TheseTListInt])
  checkAll(traverse.laws[TheseTListInt])
  checkAll(bitraverse.laws[TheseTList])

}
