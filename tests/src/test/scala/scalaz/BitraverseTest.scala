package scalaz

import scalaz.std.AllInstances.{tuple2Instance => _, _}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object BitraverseTest extends SpecLite {

  implicit val LE: Traverse[\/[*, Int]] = Bitraverse[\/].leftTraverse[Int]
  implicit val RE: Traverse[\/[Int, *]] = Bitraverse[\/].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for Either",  traverse.laws[* \/ Int])
  checkAll("Right-biased Bitraverse for Either", traverse.laws[Int \/ *])


  implicit val LT: Traverse[(*, Int)] = Bitraverse[Tuple2].leftTraverse[Int]
  implicit val RT: Traverse[(Int, *)] = Bitraverse[Tuple2].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for (,)",  traverse.laws[(*, Int)])
  checkAll("Right-biased Bitraverse for (,)", traverse.laws[(Int, *)])

  "bitraverseU" in {
    import syntax.bitraverse._
    val a: Validation[Int \/ String, Int \/ Boolean] = Success(\/-(true))
    val b = a.bitraverseU(identity, identity)
    val _ = b: (Int \/ Validation[String, Boolean])
    b must_=== \/-(Success(true))
  }

  "bisequenceU" in {
    import syntax.bitraverse._
    val a: Validation[Int \/ String, Int \/ Boolean] = Success(\/-(true))
    val b = a.bisequenceU
    val _ = b: (Int \/ Validation[String, Boolean])
    b must_=== \/-(Success(true))
  }

  "left/right bias" in {
    import scalaz.syntax.either._

    Bitraverse[\/].rightTraverse[Int].traverse(42.left[Int])(x => Vector(x + 3)) must_===(Vector(-\/(42)))
    Bitraverse[\/].leftTraverse[Int].traverse(42.left[Int])(x => Vector(x + 3))  must_===(Vector(-\/(45)))

    Bifoldable[\/].leftFoldable[Int].foldMap(42.left[Int])(identity)  must_===(42)
    Bifoldable[\/].rightFoldable[Int].foldMap(42.left[Int])(identity) must_===(0)
  }

  "both sides, left and right embedding" in {
    implicit val L_E_LO_RL: Traverse[λ[α => Option[α] \/ List[Int]]] = Bitraverse[\/].embed[Option,List].leftTraverse[Int]
    implicit val R_E_LO_RL: Traverse[λ[α => Option[Int] \/ List[α]]] = Bitraverse[\/].embed[Option,List].rightTraverse[Int]
    checkAll("Left-biased Bitraverse for Either[Option,List[_]]", traverse.laws[λ[α => Option[α] \/ List[Int]]])
    checkAll("Right-biased Bitraverse for Either[Option[_],List]", traverse.laws[λ[α => Option[Int] \/ List[α]]])

    implicit val L_E_LO: Traverse[λ[a => \/[Option[a], Int]]] = Bitraverse[\/].embedLeft[Option].leftTraverse[Int]
    implicit val R_E_RO: Traverse[λ[a => \/[Int, Option[a]]]] = Bitraverse[\/].embedRight[Option].rightTraverse[Int]
    checkAll("Left-biased Bitraverse for Either[Option,_]", traverse.laws[λ[α => Option[α] \/ Int]])
    checkAll("Right-biased Bitraverse for Either[_,Option]", traverse.laws[λ[α => Int \/ Option[α]]])
  }

}

// vim: expandtab:ts=2:sw=2
