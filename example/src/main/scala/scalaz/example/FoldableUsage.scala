package scalaz.example

object FoldableUsage extends App {
  import scalaz._
  import scalaz.std.stream._
  import scalaz.std.list._
  import scalaz.std.string._
  import scalaz.std.option._
  import scalaz.std.anyVal._
  import scalaz.std.vector._
  import scalaz.syntax.std.option._
  import scalaz.syntax.equal._

  // a continous stream of true, true, true, true, true, …
  val trues: Stream[Boolean] = Stream.continually(true)

  // we cannot use foldRight from the standard library with an
  // infinite stream, as it attempts to reverse the stream, causing a
  // Stack Overflow
  val so = \/.fromTryCatchThrowable[Boolean,Throwable](trues.foldRight(false)(_ || _))
  assert(so.fold(_.toString, _.toString) === "java.lang.StackOverflowError")

  // however, the Foldable typeclass has an implementation named foldr
  // which traverses the stream from left to right lazily, so, given a
  // function which is lazy in the right argument, we can use a foldr
  // on an infinite stream.
  def lazyOr(x: Boolean)(y: => Boolean) = x || y
  assert(Foldable[Stream].foldr(trues, false)(lazyOr))

  // when we have an available Monoid for the parameterized type, we
  // can collapse the Foldable using the monoid:
  val digits = List(0,1,2,3,4,5,6,7,8,9)
  assert(Foldable[List].fold(digits) === 45)

  // we can also map the structure to values for which we have a
  // monoid, we can collapse the list, as we can see, this one is also
  // properly lazy, allowing us to collapse our infinite stream again
  assert(Tag.unwrap(Foldable[Stream].foldMap(trues)((b: Boolean) => Tags.Disjunction(b))))

  // We can import syntax for foldable, allowing us to "enhance" the foldable with the new methods:
  import scalaz.syntax.foldable._
  assert(trues.foldr(false)(lazyOr))
  assert(Tag.unwrap(trues.foldMap((b: Boolean) => Tags.Disjunction(b))))
  assert(digits.map(_.toString).intercalate(",") === "0,1,2,3,4,5,6,7,8,9")
  assert(digits.maximum === Some(9))
  assert(digits.minimum === Some(0))

  // Foldables can be composed:
  val FoldListOfOptions = Foldable[List] compose Foldable[Option]

  val listOfOptions: List[Option[Int]] = List(1.some, 2.some, none[Int], 3.some, 4.some)
  assert(FoldListOfOptions.fold(listOfOptions) === 10)

  // with this you get a collapse function which is perhaps like the
  // flatten method in the standard library, however it is more versatile
  assert(FoldListOfOptions.collapse[List, Int](listOfOptions) === listOfOptions.flatten)

  // we can accumulate into any type for which we have an
  // ApplicativePlus instance, so here we can collapse our List of
  // Options into a Vector
  assert(FoldListOfOptions.collapse[Vector, Int](listOfOptions) === Vector(listOfOptions.flatten: _*))

  // we can go deeeeep:
  val deepFolder = Foldable[List] compose Foldable[Vector] compose Foldable[Stream] compose Foldable[Option]
  val deep: List[Vector[Stream[Option[Int]]]] = List(Vector(Stream(1.some, none[Int]), Stream(2.some)), Vector(Stream(3.some)))
  assert(deepFolder.fold(deep) === 6)
  assert(deepFolder.collapse[IList, Int](deep) === IList(1,2,3))
  assert(deepFolder.foldLeft(deep, "")(_ + _.toString) === "123")

  // Monadic Folds: we can fold over a structure with a function
  // which returns its value in a Monad,
  val sumEvens: (Int,Int) => Option[Int] = { (x, y) =>
    // if the right int is even, add it to the left
    // otherwise return None
    if((y % 2) == 0) Some(x+y) else None
  }

  // all numbers are even, so we end up with Some
  val allEvens = List(2,4,6,8,10)
  assert(allEvens.foldLeftM[Option,Int](0)(sumEvens) === Some(30))

  // when the 7 is encountered, the entire computation results in None
  val notAllEvens = List(2,4,7,8,10)
  assert(notAllEvens.foldLeftM[Option,Int](0)(sumEvens) === None)
}

