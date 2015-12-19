package scalaz

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object ApplicativeTest extends SpecLite {

  // In c44c206461fe, the functions `replicateM`, `replicateM_`, `filterM`
  // and `partitionM` have been generalized from `Monad` to `Applicative`.
  // We compare the old with the new implementation here.

  import std.list._
  import std.option._
  import std.anyVal._
  import syntax.std.list._
  import syntax.applicative._

  def replicateM[F[_] : Monad, A](n: Int, fa: F[A]): F[List[A]] =
    listInstance.sequence(List.fill(n)(fa))

  def filterM[F[_] : Monad, A](l: List[A], f: A => F[Boolean]): F[List[A]] =
    l match {
      case Nil => Monad[F].point(List())
      case h :: t => Monad[F].bind(f(h))(b => Monad[F].map(filterM(t, f))(t => if (b) h :: t else t))
    }

  "replicateM is the same" ! forAll { (fa: Option[Int]) => forAll(Gen.choose(0, 100)) { n =>
    fa.replicateM(n) must_===(replicateM(n, fa))
  }}

  "filterM is the same" ! forAll { (l: List[Int]) =>
    // don't make `None` too likely
    def pred(n: Int) = if (n < 0 && n % 2 == 0) None else Some(n % 2 == 0)
    l.filterM(pred) must_===(filterM(l, pred))
  }

}

// vim: expandtab:ts=2:sw=2
