package scalaz.example

object TraverseUsage extends App {
  import scalaz._

  import scalaz.std.list._
  import scalaz.std.vector._
  import scalaz.std.option._
  import scalaz.std.anyVal._
  import scalaz.std.string._
  import scalaz.syntax.equal._      // for === syntax
  import scalaz.syntax.validation._ // for .success and .failure syntax

  // An easy to understand first step in using the Traverse typeclass
  // is the sequence operation, which given a Traverse[F] and
  // Applicative[G] turns F[G[A]] into G[F[A]].  This is like "turning
  // the structure 'inside-out'":
  val list1: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4))
  assert(Traverse[List].sequence(list1) === Some(List(1,2,3,4)))

  // The effect of the inner Applicative is used, so in the case of
  // the Option applicative, if any of the values are None instead of
  // Some, the result of the entire computation is None:
  val list2: List[Option[Int]] = List(Some(1), Some(2), None, Some(4))
  assert(Traverse[List].sequence(list2) === None)

  // importing the traverse syntax will enhance values which have an
  // available traverse instance:
  import scalaz.syntax.traverse._
  assert(list1.sequence === Some(List(1,2,3,4)))
  assert(list1.sequence.sequence === list1)

  // A next step in using the Traverse TypeClass is the traverse
  // method. The traverse method maps function over a structure
  // through the effects of the inner applicative. You can think of
  // this method as combining a map with a sequence, so when you find
  // yourself calling fa.map(f).sequence, it can be replaced with just
  // fa.traverse(f):
  val smallNumbers = List(1,2,3,4,5)
  val bigNumbers = List(10,20,30,40,50)
  val doubleSmall: Int => Option[Int] = (x => if(x < 30) Some(x*2) else None)

  assert(smallNumbers.traverse(doubleSmall) === Some(List(2,4,6,8,10)))
  assert(smallNumbers.traverse(doubleSmall) === smallNumbers.map(doubleSmall).sequence)

  // when we hit the 30, we get a None, which "fails" the whole computation
  assert(bigNumbers.traverse(doubleSmall) === none[List[Int]])
  assert(bigNumbers.traverse(doubleSmall) === bigNumbers.map(doubleSmall).sequence)

  // both sequence and traverse come in "Unapply" varieties: sequenceU
  // and traverseU, these are useful in the case when the scala
  // compiler fails to infer an implicit Applicative instance for the
  // "inner" type. This will commonly happen when there is a "Kind
  // mismatch", for example with Validation, which is kind *,* -> *
  // instead of the expected * -> * kind of an Applicative, since the
  // Validation type constructor takes two arguments instead of one.

  val validations: Vector[ValidationNel[String,Int]] = Vector(1.success, "failure2".failureNel, 3.success, "failure4".failureNel)

  // this would not compile:
  // val result = validations.sequence

  // it gives you the perhaps hard to understand error:
  // could not find implicit value for parameter ev: scalaz.Leibniz.===[scalaz.Validation[String,Int],G[B]

  // these however work:
  val result: ValidationNel[String, Vector[Int]] = validations.sequenceU
  assert(result === NonEmptyList("failure2","failure4").failure[Vector[Int]])

  val onlyEvenAllowed: Int => ValidationNel[String, Int] = x => if(x % 2 === 0) x.successNel else (x.toString + " is not even").failureNel

  val evens = IList(2,4,6,8)
  val notAllEvens = List(1,2,3,4)

  assert(evens.traverseU(onlyEvenAllowed) === IList(2,4,6,8).success)
  assert(notAllEvens.traverseU(onlyEvenAllowed) === NonEmptyList("1 is not even","3 is not even").failure)

  // there is a traverseS method which allows us to traverse a
  // structure with a function while carrying a state through the
  // computation.

  import scalaz.State._
  // state stores the last seen Int, returns whether of not the current was a repeat
  val checkForRepeats: Int => State[Option[Int], Boolean] = { next =>
    for {
      last <- get
      _ <- put(some(next))
    } yield (last === some(next))
  }

  val nonRepeating = List(1,2,3,4)
  val repeating = List(1,2,3,3,4)

  // traverse the lists with None as the starting state, we get back a
  // list of Booleans meaning "this element was a repeat of the
  // previous
  val res1: List[Boolean] = nonRepeating.traverseS(checkForRepeats).eval(None)
  val res2: List[Boolean] = repeating.traverseS(checkForRepeats).eval(None)

  assert(Tag.unwrap(res1.foldMap(Tags.Disjunction(_))) === false)
  assert(Tag.unwrap(res2.foldMap(Tags.Disjunction(_))) === true)

  // Here's a variation of above which might be a bit of a head
  // scratcher, but this works because a Monoid gives rise to an
  // Applicative Functor.  Because Boolean is not a * -> * type
  // constructor, we need traverseU instead of traverse to find the
  // Applicative.
  import scalaz.Applicative.monoidApplicative
  assert(Tag.unwrap(res1.traverseU(Tags.Disjunction(_))) === false)
  assert(Tag.unwrap(res2.traverseU(Tags.Disjunction(_))) === true)
}
