package scalaz.example

import scalaz.{Foldable1,IList,NonEmptyList,OneAnd}
import scalaz.syntax.foldable1._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._

object Foldable1Usage extends App {
  // Foldable1 is a typeclass which is like the Foldable typeclass
  // except is only defined for structures which guarantee that there
  // is always at least one value of the contained type
  // present. Although there is a Foldable1 instance for NonEmptyList,
  // there is not one for List.

  // A quintessential method of the Foldable1 typeclass is foldMap1:
  // def foldMap1[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): B
  // this very similar to Foldable#foldMap, the difference being that
  // the restriction on F is relaxed from Monoid to Semigroup.

  // With the identity function, foldMap just collapses the contained
  // values using the semigroup append:
  assert(Foldable1[NonEmptyList].foldMap1(NonEmptyList(1,2,3))(identity) === 6)
  assert(Foldable1[NonEmptyList].foldMap1(NonEmptyList("1","2","3"))(identity) === "123")
  assert(NonEmptyList(1,2,3).foldMap1(identity) === 6)
  assert(NonEmptyList("1","2","3").foldMap1(identity) === "123")

  // or with a slightly less trivial function:
  assert(Foldable1[NonEmptyList].foldMap1(NonEmptyList(1,2,3))(_.toString) === "123")
  assert(NonEmptyList(1,2,3).foldMap1(_.toString) === "123")

  // Here's a simple binary tree definition.
  sealed trait Tree[A]
  sealed case class N[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  sealed case class L[A](a: A) extends Tree[A]

  // There are right and left associative versions of foldMap, which
  // instead of using a semigroup, take a function. Using these to
  // create our above tree can create a right leaning, or a left
  // leaning tree to demonstrate the differing associativity.

  //  /\
  // 1 /\
  //  2 /\
  //   3  4
  assert(NonEmptyList(1,2,3,4).foldMapRight1[Tree[Int]](L.apply)((a,b) ⇒ N(L(a),b)) == N(L(1), N(L(2), N(L(3), L(4)))))

  //    /\
  //   /\ 4
  //  /\ 3
  // 1  2
  assert(NonEmptyList(1,2,3,4).foldMapLeft1[Tree[Int]](L.apply)((b,a) ⇒ N(b,L(a))) == N(N(N(L(1),L(2)),L(3)),L(4)))

  // There are functions to find minimal and maximal elements similar
  // to Foldable, however, in contrast, the Foldable1 versions return
  // an A instead of an Option[A], since there is always at least one value.
  assert(NonEmptyList(1,2,3,4).minimum === Some(1))
  assert(Foldable1[NonEmptyList].minimum1(NonEmptyList(1,2,3,4)) === 1)
  assert(NonEmptyList(1,2,3,4).minimum1 === 1)

  assert(NonEmptyList(1,2,3,4).maximum === Some(4))
  assert(Foldable1[NonEmptyList].maximum1(NonEmptyList(1,2,3,4)) === 4)
  assert(NonEmptyList(1,2,3,4).maximum1 === 4)

  assert(NonEmptyList("a", "aa", "aaa").minimumBy(_.length) === Some("a"))
  assert(Foldable1[NonEmptyList].minimumBy1(NonEmptyList("a", "aa", "aaa"))(_.length) === "a")
  assert(NonEmptyList("a", "aa", "aaa").minimumBy1(_.length) === "a")

  assert(NonEmptyList("a", "aa", "aaa").maximumBy(_.length) === Some("aaa"))
  assert(Foldable1[NonEmptyList].maximumBy1(NonEmptyList("a", "aa", "aaa"))(_.length) === "aaa")
  assert(NonEmptyList("a", "aa", "aaa").maximumBy1(_.length) === "aaa")

  // Another notable instance of Foldable1 is OneAnd. OneAnd is a
  // generic container for values which are guaranteed to have at
  // least one value present. In fact, scalaz defined this type alias:
  //   type NonEmptyIList[A] = OneAnd[IList,A]
  // which gives you a structure isomorphic to a NonEmptyList but
  // using IList instead of List as the tail.
  assert(OneAnd(1, IList(2,3)).foldMap1(identity) === 6)
}
