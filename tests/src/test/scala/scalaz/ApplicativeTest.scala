package scalaz

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalazArbitrary._


object ApplicativeTest extends SpecLite {

  // In c44c206461fe, the functions `replicateM`, `replicateM_`, `filterM`
  // and `partitionM` have been generalized from `Monad` to `Applicative`.
  // We compare the old with the new implementation here.

  import std.list._
  import std.option._
  import std.string._
  import std.anyVal._
  import syntax.all._
  import IList._
  import ==>>._

  def replicateM[F[_] : Monad, A](n: Int, fa: F[A]): F[IList[A]] =
    Traverse[IList].sequence(IList.fill(n)(fa))

  def filterMList[F[_] : Monad, A](l: IList[A], f: A => F[Boolean]): F[IList[A]] =
    l match {
      case INil() => Monad[F].point(INil())
      case ICons(h, t) => Monad[F].bind(f(h))(b => Monad[F].map(filterMList(t, f))(t => if (b) h :: t else t))
    }


  def filterMMap[F[_]: Monad, A: Order, B](map: A ==>> B, f: B => F[Boolean]): F[A ==>> B] = map match {
    case Tip() => Monad[F].pure(==>>.empty)
    case Bin(kx, x, l ,r) => for {
      b <- f(x)
      newL <- filterMMap(l, f)
      newR <- filterMMap(r, f)
      self <- if (b) Monad[F].pure(Bin(kx, x, newL, newR)) else Monad[F].pure(newL.union(newR))
    } yield self
  }

  "replicateM is the same" ! forAll { (fa: Option[Int]) => forAll(Gen.choose(0, 100)) { n =>
    fa.replicateM(n) must_===(replicateM(n, fa))
  }}

  "filterM for Lists is the same" ! forAll { (l: IList[Int]) =>
    // don't make `None` too likely
    def pred(n: Int) = if (n < 0 && n % 2 == 0) None else Some(n % 2 == 0)
    l.filterM(pred) must_===(filterMList(l, pred))
  }


  "filterM for Maps is the same" ! forAll { (map: Int ==>> Int) =>
    // don't make `None` too likely
    def pred(n: Int) = if (n < 0 && n % 2 == 0) None else Some(n % 2 == 0)
    map.filterM(pred) must_===(filterMMap(map, pred))
  }

  "plusA" in {
    Applicative[List].plusA(1 :: 2 :: Nil, 10 :: 20 :: Nil) must_== 11 :: 21 :: 12 :: 22 :: Nil
    Applicative[List].plusA("1" :: "2" :: Nil, "10" :: "20" :: Nil) must_== "110" :: "120" :: "210" :: "220" :: Nil
    Applicative[List].plusA("1" :: Nil, Nil) must_== Nil
    Applicative[List].plusA(Nil, "1" :: Nil) must_== Nil
    Applicative[List].plusA[String](Nil, Nil) must_== Nil

    Applicative[Option].plusA(Option(1), Option(2)) must_== Some(3)
    Applicative[Option].plusA(Option("1"), Option("2")) must_== Some("12")
    Applicative[Option].plusA(Option("1"), Option.empty[String]) must_== None
    Applicative[Option].plusA(Option.empty[String], Some("1")) must_== None
    Applicative[Option].plusA(Option.empty[String], Option.empty[String]) must_== None

    //Const is an applicative that is not a monad
    def const[A](a: A) = Const[A, String](a) // won't compile w/o known B
    Applicative[Const[Int, *]].plusA(const(1), const(2)) must_== const(3)
    Applicative[Const[String, *]].plusA(const("1"), const("2")) must_== const("12")

    // check +++ syntax

    (1 :: Nil) +++ (10 :: Nil) must_== 11 :: Nil
    List("Gurren ") +++ List("Lagann") must_=== List("Gurren Lagann")
    List("Gurren ") +++ List.empty must_=== List.empty[String]
    List.empty +++ List("Lagann") must_=== List.empty[String]
    List.empty[String] +++ List.empty[String] must_=== List.empty[String]

    Option("Gurren ") +++ Option("Lagann") must_=== Option("Gurren Lagann")
    Option(5) +++ Option(3) must_=== Option(8)
    None +++ Option(3) must_=== Option.empty[Int]
    Option(5) +++ None must_=== Option.empty[Int]
    Option.empty[Int] +++ Option.empty[Int] must_=== Option.empty[Int]

    const(5) +++ const(3) must_=== const(8)
    const("5") +++ const("3") must_=== const("53")
  }

}

// vim: expandtab:ts=2:sw=2
