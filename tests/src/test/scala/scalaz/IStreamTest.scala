package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.contravariant._
import syntax.foldable._
import org.scalacheck.Prop.forAll

object IStreamTest extends SpecLite {

  checkAll(monadPlus.strongLaws[IStream])
  checkAll(isEmpty.laws[IStream])
  checkAll(traverse.laws[IStream])

  implicit def iStreamShow[A: Show]: Show[IStream[A]] =
    Show[List[A]].contramap(_.toList)

  "reverse" ! forAll{ e: IStream[Int] =>
    e.reverse.toList must_===(e.toList.reverse)
    e.reverse.reverse must_===(e)
  }

  "Foldable.foldLeft" ! forAll{ xs: List[List[Int]] =>
    Foldable[IStream].foldLeft(IStream.fromFoldable(xs), List[Int]())(_ ::: _) must_===(xs.foldLeft(List[Int]())(_ ::: _))
  }

  "foldMap evaluates lazily" in {
    Foldable[IStream].foldMap(IStream.Lazy.infinite(false))(identity)(booleanInstance.conjunction) must_===(false)
  }

  // https://github.com/scalaz/scalaz/issues/1515
  "traverse is curiously lazy over Id" ! {
    import syntax.traverse._
    import Scalaz.Id

    val stream = IStream.Lazy.infinite(1)
    val _ = stream.traverse[Id, Int](identity)
    // got here without exception... that's good!
  }

}
