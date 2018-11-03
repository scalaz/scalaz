package scalaz

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalazArbitrary.ilistArbitrary
import scalaz.std.int._
import scalaz.std.string._


object ApplicativeTest extends SpecLite {

  // In c44c206461fe, the functions `replicateM`, `replicateM_`, `filterM`
  // and `partitionM` have been generalized from `Monad` to `Applicative`.
  // We compare the old with the new implementation here.

  import std.list._
  import std.option._
  import std.anyVal._
  import syntax.applicative._
  import syntax.traverse._
  import IList._

  def replicateM[F[_] : Monad, A](n: Int, fa: F[A]): F[IList[A]] =
    Traverse[IList].sequence(IList.fill(n)(fa))

  def filterM[F[_] : Monad, A](l: IList[A], f: A => F[Boolean]): F[IList[A]] =
    l match {
      case INil() => Monad[F].point(INil())
      case ICons(h, t) => Monad[F].bind(f(h))(b => Monad[F].map(filterM(t, f))(t => if (b) h :: t else t))
    }

  "replicateM is the same" ! forAll { (fa: Option[Int]) => forAll(Gen.choose(0, 100)) { n =>
    fa.replicateM(n) must_===(replicateM(n, fa))
  }}

  "filterM is the same" ! forAll { (l: IList[Int]) =>
    // don't make `None` too likely
    def pred(n: Int) = if (n < 0 && n % 2 == 0) None else Some(n % 2 == 0)
    l.filterM(pred) must_===(filterM(l, pred))
  }

  "+++" in {
    Option(5) +++ Option(3)                   must_=== Option(8)
    None +++ Option(3)                        must_=== Option.empty[Int]
    Option(5) +++ None                        must_=== Option.empty[Int]
    Option.empty[Int] +++ Option.empty[Int]   must_=== Option.empty[Int]

    List("Gurren ") +++ List("Lagann")        must_=== List("Gurren Lagann")
    List("Gurren ") +++ List.empty            must_=== List.empty[String]
    List.empty +++ List("Lagann")             must_=== List.empty[String]
    List.empty[String] +++ List.empty[String] must_=== List.empty[String]

    def const(i: Int) = Const[Int, String](i) // won't compile w/o known B
    const(5) +++ const(3)                     must_=== const(8)
  }

}

// vim: expandtab:ts=2:sw=2
