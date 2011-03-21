package scalaz.example

// Purely functional typed mutable state threads
object ExampleST {
  import scalaz._
  import Scalaz._
  import ST._
  import Forall._

  // Creates a new mutable reference and mutates it
  def e1[A] = for {
    r <- newVar[A, Int](0)
    x <- r.mod(_ + 1)
  } yield x

  // Creates a new mutable reference, mutates it, and reads its value.
  def e2[A] = e1[A].flatMap(_.read)

  // Run e2, returning the final value of the mutable reference.
  def test = Forall[({type λ[S] = ST[S, Int]})#λ](
    (k : DNE[({type λ[S] = ST[S, Int]})#λ]) => k(e2))

  // Will not compile because it exposes a mutable variable.
  // def test2 = Forall[({type λ[S] = ST[S, STRef[S, Int]]})#λ](
  //   (k: DNE[({type λ[S] = ST[S, STRef[S, Int]]})#λ]) => k(e1))

  // Bin-sort a list into an immutable array.
  // Uses a non-observable mutable array in the background.
  def binSort[A: Manifest](size: Int, key: A => Int, as: List[A]): ImmutableArray[List[A]] =   
    accumArray(size, (vs: List[A], v: A) => v :: vs, List(), for { a <- as } yield (key(a), a))
}

