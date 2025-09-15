package scalaz

import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances.{ enumInstance => _, _}
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll
import Cofree._
import Cofree.CofreeZip
import Isomorphism._
import EphemeralStream._

object CofreeTest extends SpecLite {

  type CofreeLazyOption[A] = Cofree[LazyOption, A]
  type CofreeStream[A] = Cofree[EStream, A]
  type OneAndStream[A] = OneAnd[EStream, A]
  type OneAndList[A] = OneAnd[List, A]
  type CofreeOption[A] = Cofree[Option, A]

  val oneAndListCofreeOptionIso: OneAndList <~> CofreeOption =
    new IsoFunctorTemplate[OneAndList, CofreeOption] {
      def to_[A](fa: OneAndList[A]) =
        Cofree.unfold(fa) {
          case OneAnd(a, h :: t) =>
            (a, Some(OneAnd(h, t)))
          case OneAnd(a, _) => (a, None)
        }
      def from_[A](ga: CofreeOption[A]) =
        OneAnd(
          ga.head,
          ga.tail.map(s =>
            Foldable[CofreeOption].foldRight(s, List.empty[A])(_ :: _)
          ).getOrElse(Nil)
        )
    }

  val oneAndStreamCofreeLazyOptionIso: OneAndStream <~> CofreeLazyOption =
    new IsoFunctorTemplate[OneAndStream, CofreeLazyOption] {
      def to_[A](fa: OneAndStream[A]) =
        Cofree.unfold(fa){
          case OneAnd(a, h ##:: t) => (a, LazyOption.lazySome(OneAnd(h, t)))
          case OneAnd(a, _)       => (a, LazyOption.lazyNone)
        }
      def from_[A](fa: CofreeLazyOption[A]) =
        OneAnd(
          fa.head,
          fa.tail.map(s =>
            Foldable[CofreeLazyOption].foldRight(s, emptyEphemeralStream[A])(_ ##:: _)
          ).getOrElse(emptyEphemeralStream)
        )
    }

  val treeCofreeStreamIso: Tree <~> CofreeStream =
    new IsoFunctorTemplate[Tree, CofreeStream] {
      def to_[A](tree: Tree[A]): CofreeStream[A] =
        Cofree(tree.rootLabel, tree.subForest.map(to(_)))
      def from_[A](c: CofreeStream[A]): Tree[A] =
        Tree.Node(c.head, c.tail.map(from(_)))
    }

  implicit def CofreeLazyOptionArb[A: Arbitrary]: Arbitrary[CofreeLazyOption[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[OneAndStream[A]]])(oneAndStreamCofreeLazyOptionIso.to(_))

  implicit def CofreeStreamArb[A: Arbitrary]: Arbitrary[CofreeStream[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[Tree[A]]])(treeCofreeStreamIso.to.apply)

  implicit def CofreeLazyOptionCogen[A: Cogen]: Cogen[CofreeLazyOption[A]] =
    implicitly[Cogen[OneAndStream[A]]].contramap(oneAndStreamCofreeLazyOptionIso.from.apply)

  implicit def CofreeStreamCogen[A: Cogen]: Cogen[CofreeStream[A]] =
    implicitly[Cogen[Tree[A]]].contramap(treeCofreeStreamIso.from.apply)

  implicit def CofreeOptionCogen[A: Cogen]: Cogen[CofreeOption[A]] =
    implicitly[Cogen[OneAndList[A]]].contramap(oneAndListCofreeOptionIso.from.apply)

  implicit def CofreeOptionArb[A: Arbitrary]: Arbitrary[CofreeOption[A]] = {
    import org.scalacheck.Arbitrary._
    import org.scalacheck.Gen
    val arb = Arbitrary { Gen.listOfN(20, implicitly[Arbitrary[A]].arbitrary ) }
    Functor[Arbitrary].map(arb){
      case h :: Nil => oneAndListCofreeOptionIso.to( OneAnd(h, Nil))
      case h :: t => oneAndListCofreeOptionIso.to( OneAnd(h, t) )
    }
  }

  checkAll("CofreeLazyOption", comonad.laws[CofreeLazyOption])
  checkAll("CofreeLazyOption", traverse1.laws[CofreeLazyOption])
  checkAll("CofreeLazyOption", monad.laws[CofreeLazyOption])
  checkAll("CofreeLazyOption", equal.laws[CofreeLazyOption[Int]])

  checkAll("CofreeStream", comonad.laws[CofreeStream])
  checkAll("CofreeStream", traverse1.laws[CofreeStream])
  checkAll("CofreeStream", monad.laws[CofreeStream])
  checkAll("CofreeStream", equal.laws[CofreeStream[Int]])

  checkAll("CofreeOption", comonad.laws[CofreeOption])
  checkAll("CofreeOption", monad.laws[CofreeOption])

  {
    type CofreeZipLazyOption[A] = CofreeZip[LazyOption, A]

    implicit def CofreeZipLazyOptionArb[A: Arbitrary]: Arbitrary[CofreeZipLazyOption[A]] =
      Tags.Zip.subst(CofreeLazyOptionArb[A])

    // Hack: avoid stack overflow because `Applicative[CofreeLazyOption].point` is infinite stream
    def CofreeZipLazyOptionEqual[A: Equal]: Equal[CofreeZipLazyOption[A]] =
      Equal.equalBy{ a =>
        val OneAnd(h, t) = oneAndStreamCofreeLazyOptionIso.from(Tag.unwrap(a))
        h -> t.take(1000)
      }

    checkAll("CofreeZipLazyOption", applicative.laws[CofreeZipLazyOption](using implicitly, implicitly, implicitly, CofreeZipLazyOptionEqual))
  }

  {
    type CofreeZipStream[A] = CofreeZip[EStream, A]

    implicit def CofreeZipStreamArb[A: Arbitrary]: Arbitrary[CofreeZipStream[A]] =
      Tags.Zip.subst(CofreeStreamArb[A])

    checkAll("CofreeZipStream", ScalazProperties.apply.laws[CofreeZipStream])
  }

  "no stack overflow Applicative[CofreeZip[IList, *]]#point" in {
    val a = 1
    val b = Applicative[CofreeZip[IList, *]].point(a)
    val size = 10
    Foldable[Cofree[IList, *]].toLazyList(Tag.unwrap(b)).take(size) must_=== LazyList.fill(size)(a)
  }

  "Applicative[λ[α => CofreeZip[LazyOption, α]]] is Applicative[λ[α => Stream[α] @@ Zip]]" ! forAll{
    (a: OneAndStream[Int], b: OneAndStream[Int]) =>

    import syntax.foldable._
    val f = (_: Int) + (_: Int)
    val (h ##:: t) : EphemeralStream[Int] = Tag.unwrap(Applicative[λ[α => EphemeralStream[α] @@ Tags.Zip]].apply2(Tags.Zip[EStream[Int]](a.toEphemeralStream), Tags.Zip[EStream[Int]](b.toEphemeralStream))(f))

    val aa = Tags.Zip(oneAndStreamCofreeLazyOptionIso.to(a))
    val bb = Tags.Zip(oneAndStreamCofreeLazyOptionIso.to(b))
    val y = Applicative[λ[α => CofreeZip[LazyOption, α]]].apply2(aa, bb)(f)
    OneAnd(h, t) must_=== oneAndStreamCofreeLazyOptionIso.from(Tag.unwrap(y))
  }

  "no stack overflow unfoldC, mapBranching" in {
    import syntax.foldable._
    val n = 100
    val list = Cofree.unfoldC(1)(a => Option(a + 1)).mapBranching(NaturalTransformation.refl).toLazyList.take(n).toList
    list must_=== (1 to n).toList
  }

  object instances{
    def comonad[F[_]: Functor] = Comonad[Cofree[F, *]]
    def bind[F[_]: Plus: Functor] = Bind[Cofree[F, *]]
    def monad[F[_]: PlusEmpty: Functor] = Monad[Cofree[F, *]]
    def foldable1[F[_]: Foldable] = Foldable1[Cofree[F, *]]
    def traverse1[F[_]: Traverse] = Traverse1[Cofree[F, *]]

    // checking absence of ambiguity
    def bind[F[_]: PlusEmpty: Functor] = Bind[Cofree[F, *]]
    def bind[F[_]: PlusEmpty: Traverse] = Bind[Cofree[F, *]]
    def functor[F[_]: Traverse] = Functor[Cofree[F, *]]
    def functor[F[_]: Traverse1] = Functor[Cofree[F, *]]
    def functor[F[_]: Plus: Functor] = Functor[Cofree[F, *]]
    def functor[F[_]: PlusEmpty: Traverse] = Functor[Cofree[F, *]]
    def functor[F[_]: PlusEmpty: Traverse1] = Functor[Cofree[F, *]]
    def foldable1[F[_]: Traverse1] = Foldable1[Cofree[F, *]]
    def traverse1[F[_]: Traverse1] = Traverse1[Cofree[F, *]]

    object zip{
      def functor[F[_]: Functor] = Functor[CofreeZip[F, *]]
      def apply[F[_]: Apply] = Apply[CofreeZip[F, *]]
      def applicative[F[_]: Applicative] = Applicative[CofreeZip[F, *]]

      // checking absence of ambiguity
      def functor[F[_]: Applicative] = Functor[CofreeZip[F, *]]
      def apply[F[_]: Applicative] = Apply[CofreeZip[F, *]]
    }

  }
}
