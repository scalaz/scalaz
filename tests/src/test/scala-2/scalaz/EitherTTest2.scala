package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object EitherTTest2 extends SpecLite {

  "rightU" should {
    val a: String \/ Int = \/-(1)
    val b: EitherT[({type l[a] = String \/ a})#l, Boolean, Int] = EitherT.rightU[Boolean](a)
    b must_== EitherT.rightT[({type l[a] = String \/ a})#l, Boolean, Int](a)
  }

  "either, pureLeft, pure" ! forAll { (a: String \/ Int) =>
    val e = EitherT.eitherT(Option(a))

    e must_=== {
      a match {
        case -\/(v) => EitherT.pureLeft(v)
        case \/-(v) => EitherT.pure(v)
      }
    }

    e must_=== EitherT.either(a)
  }

  "eitherT, leftT, rightT syntax" ! forAll { (a: String \/ Int) =>
    import scalaz.syntax.eithert._

    val e = EitherT.eitherT(Option(a))

    e must_=== {
      a match {
        case -\/(v) => v.leftT
        case \/-(v) => v.rightT
      }
    }

    e must_=== a.eitherT
  }

  def compilationTests() = {
    // compilation test
    // https://gist.github.com/vmarquez/5106252/
    {
      import scalaz.syntax.either._

      case class ABC(s:String)

      implicit val m = new Monoid[(ABC, Int)] {
        def zero: (ABC, Int) = (null, -1)
        def append(f1: (ABC, Int), f2: => (ABC, Int)): (ABC, Int) = f1
      }

      def brokenMethod: EitherT[Option, (ABC, Int), (ABC, String)] =
        EitherT(Some((ABC("abcData"),"Success").right))

      def filterComp =
        brokenMethod
        .filter {
          case (abc,"Success") => true
          case _ => false
        }.map {
          case (abc, "Success") => "yay"
        }

      for {
        (a,b) <- brokenMethod
      } yield "yay"
    }

    //compilation test for eitherTU
    {
      val se: State[Vector[String], Int \/ Float] = null
      EitherT.eitherTU(se)
      val ee: String \/ (Int \/ Float) = null
      EitherT.eitherTU(ee)
    }
  }
}
