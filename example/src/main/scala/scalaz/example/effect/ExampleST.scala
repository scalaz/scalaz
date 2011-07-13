package scalaz.example
package effect

// Purely functional typed mutable state threads
object ExampleST {

  import scalaz._
  import Scalaz._
  import effect._

  // Creates a new mutable reference and mutates it
  def e1[A] = for {
    r <- newVar[A](0)
    x <- r.mod(_ + 1)
  } yield x

  // Creates a new mutable reference, mutates it, and reads its value.
  def e2[A] = e1[A].flatMap(_.read)

  // Run e2, returning the final value of the mutable reference.
  def test = new Forall[({type λ[S] = ST[S, Int]})#λ] {
    def apply[A] = e2
  }

  // Run e1, returning a mutable reference to the outside world.
  // The type system ensures that this can never be run.
  def test2 = new Forall[({type λ[S] = ST[S, STRef[S, Int]]})#λ] {
    def apply[A] = e1
  }

  val compiles = runST(test)

  // Does not compile because it would expose a mutable reference.
  // val doesNotCompile = runST(test2)

  // Bin-sort a list into an immutable array.
  // Uses a non-observable mutable array in the background.
  def binSort[A: Manifest](size: Int, key: A => Int, as: List[A]): ImmutableArray[List[A]] =
    accumArray(size, (vs: List[A], v: A) => v :: vs, List(), for {a <- as} yield (key(a), a))
}

