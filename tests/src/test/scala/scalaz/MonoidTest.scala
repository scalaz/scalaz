package scalaz

import std.AllInstances._
import org.scalacheck.Prop.forAll

object MonoidTest extends SpecLite {
  "endo multiply" in {
    import syntax.monoid._

    def inc(i: Int) = i + 1

    val incTimesThree: Endo[Int] = Endo(inc).multiply(3)
    incTimesThree(0) must_===(3)
  }

  "unfold" in {
    val ss = std.stream.unfold(1) {
      case x if x < 10 => Some((x.toString, x * 2))
      case _           => None
    }
    ss.toList must_===(List("1", "2", "4", "8"))
  }

  "intercalate empty" in (
    Foldable[List].intercalate(List[String](), "oops")
    must_===("")
  )

  "intercalate" in {
    val xs = List(Vector(Cord("this"), Cord("has")), Vector(),
		  Vector(Cord("elements")), Vector(Cord("beneath")), Vector())
    ((Foldable[List] compose Foldable[Vector]).intercalate(xs, Cord("!!")).toString
     must_===(Cord("this!!has!!elements!!beneath").toString))
  }
}
