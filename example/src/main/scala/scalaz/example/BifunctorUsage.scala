package scalaz
package example

import syntax.validation._
import syntax.either._
import syntax.bifunctor._
import syntax.equal._
import std.tuple._
import std.list._
import std.string._
import std.anyVal._
import std.either._


/**
  * A Bifunctor is very similar to a Functor, which you are hopefully
  * already familiar with. Whereas a Functor operates on a * → * and
  * has a single operation `map` which takes a function from A => B to
  * map a F[A] to a F[B], a Bifunctor operates on a *,* → * and has a
  * single operation `bimap` which takes two functions: A ⇒ C and a
  * B ⇒ D to map a F[A,B] to a F[C,D]:
  *
  * def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
  *
  * some examples of common types for which we have Bifunctor
  * instances are Either, Validation, \/, Tuple2
  */
object BifunctorUsage extends App {
  //
  // bimap
  //

  // For a tuple, the result of bimap is obvious:
  assert(Bifunctor[Tuple2].bimap(("asdf", 1))(_.toUpperCase, _+1) === ("ASDF", 2))

  // For sum types, which function is applied depends on what value is present:
  assert(Bifunctor[Either].bimap(Left("asdf") : Either[String,Int])(_.toUpperCase, _+1) === (Left("ASDF")))
  assert(Bifunctor[Either].bimap(Right(1): Either[String,Int])(_.toUpperCase, _+1) === (Right(2)))

  assert(Bifunctor[Validation].bimap("asdf".failure[Int])(_.toUpperCase, _+1) === "ASDF".failure)
  assert(Bifunctor[Validation].bimap(1.success[String])(_.toUpperCase, _+1) === 2.success)

  assert(Bifunctor[\/].bimap("asdf".left[Int])(_.toUpperCase, _+1) === "ASDF".left)
  assert(Bifunctor[\/].bimap(1.right[String])(_.toUpperCase, _+1) === 2.right)

  // There is syntax for bimap:
  assert(("asdf",1).bimap(_.length, _+1) === (4,2))

  // Bifunctors are covariant in both their type parameters, which is expressed by widen
  assert(("asdf", 1).widen[Any, Any].isInstanceOf[(Any, Any)])

  //
  // leftMap / rightMap
  //

  // There are functions to only map the "right" or "left" value only:
  assert(Bifunctor[Tuple2].leftMap(("asdf", 1))(_.substring(1)) === ("sdf", 1))
  assert(Bifunctor[Tuple2].rightMap(("asdf", 1))(_ + 3) === ("asdf", 4))

  // These come with syntax.
  assert(1.success[String].rightMap(_ + 10) === 11.success)
  assert(("a", 1).rightMap(_ + 10) === ("a",11))

  // and some even fancier syntax
  val two = 1.success[String] :-> (_ + 1)
  assert(two === 2.success)

  // On the left side, the type inference can be bad, so that we are
  // forced to be explicit about the types on the function we leftMap.
  val strlen: String => Int = _.length
  assert((strlen <-: ("asdf", 1)) === (4,1))
  assert((((_:String).length) <-: ("asdf", 1)) === (4,1))

  val fourTwo = strlen <-: ("asdf", 1) :-> (_ + 1)
  assert(fourTwo === (4,2))

  //
  // Functor composition
  //

  // We can compose a functor with a bifunctor to get a new bifunctor.
  // For example, if we have a list of a type for which we have a
  // bifunctor, we can get a bimap that operates on every item in the
  // list.
  val bff = Functor[List] bicompose Bifunctor[\/]
  val bfres = bff.bimap(List("asdf".left, 2.right, "qwer".left, 4.right))(_.toUpperCase, _+1)
  assert(bfres === List("ASDF".left, 3.right, "QWER".left, 5.right))

  //
  // Functor extraction
  //

  // We can get at the either the left or right underlying functors.
  val leftF = Bifunctor[\/].leftFunctor[String]
  assert(leftF.map("asdf".right[Int])(_ + 1) === "asdf".right)
  assert(leftF.map(1.left)(_ + 1) === 2.left)

  val rightF = Bifunctor[\/].rightFunctor[String]
  assert(rightF.map("asdf".left[Int])(_ + 1) === "asdf".left)
  assert(rightF.map(1.right)(_ + 1) === 2.right)

  //
  // Ufunctor
  //

  // If we have an F[A,A] (instead of F[A,B] with A and B different)
  // we can extract a "unified functor" which is a functor,
  assert(Bifunctor[Tuple2].uFunctor.map((2,3))(_ * 3) === (6,9))

  // or skip the step of extracting the unified functor using the umap method.
  assert(Bifunctor[Tuple2].umap((2,3))(_ * 3) === (6,9))
}
