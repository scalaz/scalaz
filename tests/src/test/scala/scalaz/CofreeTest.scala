package scalaz

import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll
import Cofree._
import Cofree.CofreeZip
import Isomorphism._

object CofreeTest extends SpecLite {

  type CofreeLazyOption[A] = Cofree[LazyOption, A]
  type CofreeStream[A] = Cofree[Stream, A]
  type OneAndStream[A] = OneAnd[Stream, A]
  type OneAndList[A] = OneAnd[List, A]
  type CofreeOption[A] = Cofree[Option, A]

  implicit def cofreeEqual[F[_], A](implicit F: Eq1[F], A: Equal[A]): Equal[Cofree[F, A]] =
    Equal.equal{ (a, b) =>
      A.equal(a.head, b.head) && F.eq1(cofreeEqual[F, A]).equal(a.tail, b.tail)
    }

  implicit def cofreeZipEqual[F[_]: Eq1, A: Equal]: Equal[CofreeZip[F, A]] =
    Tag.subst(cofreeEqual[F, A])

  //needed to prevent SOE for testing with equality
  implicit def cofreeOptEquals[A](implicit e: Equal[A]): Equal[CofreeOption[A]] = new Equal[CofreeOption[A]] {
    override def equal(a: CofreeOption[A], b: CofreeOption[A]): Boolean = {
      def tr(a: CofreeOption[A], b: CofreeOption[A]): Boolean =
        (a.tail, b.tail) match {
          case (Some(at), Some(bt)) if (e.equal(a.head, b.head)) => tr(at, bt)
          case (None, None) if (e.equal(a.head, b.head)) => true
          case _ => false
        }
      tr(a,b)
    }
  }

  val oneAndListCofreeOptionIso: OneAndList <~> CofreeOption =
    new IsoFunctorTemplate[OneAndList, CofreeOption] {
      def to[A](fa: OneAndList[A]) =
        Cofree.unfold(fa) {
          case OneAnd(a, h :: t) =>
            (a, Some(OneAnd(h, t)))
          case OneAnd(a, _) => (a, None)
        }
      def from[A](ga: CofreeOption[A]) =
        OneAnd(
          ga.head,
          ga.tail.map(s =>
            Foldable[CofreeOption].foldRight(s, List.empty[A])(_ :: _)
          ).getOrElse(Nil)
        )
    }

  val oneAndStreamCofreeLazyOptionIso: OneAndStream <~> CofreeLazyOption =
    new IsoFunctorTemplate[OneAndStream, CofreeLazyOption] {
      def to[A](fa: OneAndStream[A]) =
        Cofree.unfold(fa){
          case OneAnd(a, h #:: t) => (a, LazyOption.lazySome(OneAnd(h, t)))
          case OneAnd(a, _)       => (a, LazyOption.lazyNone)
        }
      def from[A](fa: CofreeLazyOption[A]) =
        OneAnd(
          fa.head,
          fa.tail.map(s =>
            Foldable[CofreeLazyOption].foldRight(s, Stream.empty[A])(_ #:: _)
          ).getOrElse(Stream.empty)
        )
    }

  val treeCofreeStreamIso: Tree <~> CofreeStream =
    new IsoFunctorTemplate[Tree, CofreeStream] {
      def to[A](tree: Tree[A]): CofreeStream[A] =
        Cofree(tree.rootLabel, tree.subForest.map(to))
      def from[A](c: CofreeStream[A]): Tree[A] =
        Tree.Node(c.head, c.tail.map(from(_)))
    }

  implicit def CofreeLazyOptionArb[A: Arbitrary]: Arbitrary[CofreeLazyOption[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[OneAndStream[A]]])(oneAndStreamCofreeLazyOptionIso.to(_))

  implicit def CofreeStreamArb[A: Arbitrary]: Arbitrary[CofreeStream[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[Tree[A]]])(treeCofreeStreamIso.to)

  implicit def CofreeLazyOptionCogen[A: Cogen]: Cogen[CofreeLazyOption[A]] =
    implicitly[Cogen[OneAndStream[A]]].contramap(oneAndStreamCofreeLazyOptionIso.from)

  implicit def CofreeStreamCogen[A: Cogen]: Cogen[CofreeStream[A]] =
    implicitly[Cogen[Tree[A]]].contramap(treeCofreeStreamIso.from)

  implicit def CofreeOptionCogen[A: Cogen]: Cogen[CofreeOption[A]] =
    implicitly[Cogen[OneAndList[A]]].contramap(oneAndListCofreeOptionIso.from)

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

    checkAll("CofreeZipLazyOption", applicative.laws[CofreeZipLazyOption](implicitly, implicitly, implicitly, CofreeZipLazyOptionEqual))
  }

  {
    type CofreeZipStream[A] = CofreeZip[Stream, A]

    implicit def CofreeZipStreamArb[A: Arbitrary]: Arbitrary[CofreeZipStream[A]] =
      Tags.Zip.subst(CofreeStreamArb[A])

    checkAll("CofreeZipStream", ScalazProperties.apply.laws[CofreeZipStream])
  }

  "no stack overflow Applicative[CofreeZip[IList, ?]]#point" in {
    val a = 1
    val b = Applicative[CofreeZip[IList, ?]].point(a)
    val size = 10
    Foldable[Cofree[IList, ?]].toStream(Tag.unwrap(b)).take(size) must_=== Stream.fill(size)(a)
  }

  "Applicative[λ[α => CofreeZip[LazyOption, α]]] is Applicative[λ[α => Stream[α] @@ Zip]]" ! forAll{
    (a: OneAndStream[Int], b: OneAndStream[Int]) =>

    import syntax.foldable._
    val f = (_: Int) + (_: Int)
    val h #:: t = Tag.unwrap(Applicative[λ[α => Stream[α] @@ Tags.Zip]].apply2(Tags.Zip[Stream[Int]](a.toStream), Tags.Zip[Stream[Int]](b.toStream))(f))

    val aa = Tags.Zip(oneAndStreamCofreeLazyOptionIso.to(a))
    val bb = Tags.Zip(oneAndStreamCofreeLazyOptionIso.to(b))
    val y = Applicative[λ[α => CofreeZip[LazyOption, α]]].apply2(aa, bb)(f)
    OneAnd(h, t) must_=== oneAndStreamCofreeLazyOptionIso.from(Tag.unwrap(y))
  }

  "no stack overflow unfoldC, mapBranching" in {
    import syntax.foldable._
    val n = 100
    val list = Cofree.unfoldC(1)(a => Option(a + 1)).mapBranching(NaturalTransformation.refl).toStream.take(n).toList
    list must_=== (1 to n).toList
  }

  object instances{
    def comonad[F[_]: Functor] = Comonad[Cofree[F, ?]]
    def bind[F[_]: Plus: Functor] = Bind[Cofree[F, ?]]
    def monad[F[_]: PlusEmpty: Functor] = Monad[Cofree[F, ?]]
    def foldable1[F[_]: Foldable] = Foldable1[Cofree[F, ?]]
    def traverse1[F[_]: Traverse] = Traverse1[Cofree[F, ?]]

    // checking absence of ambiguity
    def bind[F[_]: PlusEmpty: Functor] = Bind[Cofree[F, ?]]
    def functor[F[_]: PlusEmpty: Traverse] = Functor[Cofree[F, ?]]
    def foldable1[F[_]: Traverse1] = Foldable1[Cofree[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[Cofree[F, ?]]

    object zip{
      def functor[F[_]: Functor] = Functor[CofreeZip[F, ?]]
      def apply[F[_]: Apply] = Apply[CofreeZip[F, ?]]
      def applicative[F[_]: Applicative] = Applicative[CofreeZip[F, ?]]

      // checking absence of ambiguity
      def functor[F[_]: Applicative] = Functor[CofreeZip[F, ?]]
      def apply[F[_]: Applicative] = Apply[CofreeZip[F, ?]]
    }

  }
}

