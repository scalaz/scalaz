package scalaz.example

import scalaz._
import Scalaz._

object AdjunctUsage extends App {

  // two lists we'll be traversing
  val nonRepeating = List(1,2,3,4)
  val repeating = List(1,2,3,3,4)

  // Here is a function which uses the state monad to traverse a list
  // and remember the previous element of the list, in order to
  // determine if there are repeats
  val checkForRepeats: Int ⇒ State[Option[Int], Boolean] = { next ⇒
    import State._
    for {
      last ← get          // get the last value from the previous iteration
      _ ← put(some(next)) // set the next value to the value in this iteration
    } yield (last === some(next))  // emit a boolean if this is the same as last
  }

  // traverse the list with our stateful computation, producing a list
  // of booleans for "was this a repeat of the previous"
  val res1: List[Boolean] = Traverse[List].traverseS(nonRepeating)(checkForRepeats).eval(None)
  val res2: List[Boolean] = Traverse[List].traverseS(repeating)(checkForRepeats).eval(None)

  // when we collapse the lists of booleans, we expect the non-repeating list to all be false
  assert(Tag.unwrap(res1.foldMap(Tags.Disjunction(_))) === false)
  // and we expect the repeating list to have at least one true
  assert(Tag.unwrap(res2.foldMap(Tags.Disjunction(_))) === true)

  //-------------------------------------------
  // using reader and write adjunction as State

  // Writer and Reader Functors form and adjunction that gives us a
  // Monad the behaves like State, think of it as a function which
  // Reads the state perfoms a computation and Writes the new state.
  type RWState[S,A] = Reader[S, Writer[S, A]]

  // here is our checkForRepeats function rewriten for the new form
  val checkForRepeatsAdj: Int ⇒ RWState[Option[Int], Boolean] = { next ⇒
    // read in the last value, write out the next value and the boolean result
    Reader(last ⇒ Writer(some(next), last === some(next)))
  }

  // now we can perform the exact same stateful mutation, by
  // traversing over the same lists with our new Adjunction based computation
  val adjOptionInt = Adjunction.writerReaderAdjunction[Option[Int]]
  val didTheyRepeat : Boolean = {
    implicit val adjMonad = adjOptionInt.monad
    val (_, bools) = nonRepeating.traverseU(checkForRepeatsAdj).run(None).run
    bools.foldLeft(false)((r, a) ⇒ r || a)
  }
  assert(didTheyRepeat == false)

  val didTheyRepeat2 : Boolean = {
    implicit val adjMonad = adjOptionInt.monad
    val (_, bools) = repeating.traverseU(checkForRepeatsAdj).run(None).run
    bools.foldLeft(false)((r, a) ⇒ r || a)
  }
  assert(didTheyRepeat2 === true)

  // here's another stateful computation, it simply carries a sum
  // along in as the State, returning Unit. This could clearly be done
  // more simply as a fold, but we'll use this again later more
  // meaningfully
  val adjInt = Adjunction.writerReaderAdjunction[Int]
  val sum: Int ⇒ RWState[Int,Unit] = { next ⇒
    Reader(last ⇒ Writer(last + next, ()))
  }

  val sumOfInts = {
    implicit val adjMonad = adjInt.monad
    nonRepeating.traverseU(sum).run(0).run._1
  }

  assert(sumOfInts === 10)


  // the Id functors get in the way of type inference
  // TODO: see if we can get rid of this explicitness
  // TODO: also see if we can Trampoline these as they are going to
  //   blow the stack for a long traversal

  // We can create a type which represents both of the above
  // computations: look for repeats and sum the ints, could be
  // composed together, so that they happen on a single pass through a
  // Traversable,
  type ROIRIWW[A] = Reader[Option[Int],      // read the previous value for computing repeats
                           Reader[Int,       // read the accumulated sum
                                  Writer[Int,// write the new sum
                                         Writer[Option[Int],A]]]] // write the next value for computing repeats

  // now we can combine our two stateful computations
  val checkForRepeatsAdjAndSum2: Int ⇒ ROIRIWW[Boolean] = { next ⇒
    checkForRepeatsAdj(next).map(w ⇒ sum(next).map(_.map(_ ⇒ w)))
  }

  // but this can be done more generically for any two of these Reader/Writer adjunctions
  def run2RWState[A,S1,S2,B,C,R](rws1: A ⇒ RWState[S1,B], rws2: A ⇒ RWState[S2,C], f: (B,C) ⇒ R) = { a: A ⇒
    rws1(a).map(b ⇒ rws2(a).map(_.map(c ⇒ Writer(b.run._1,f(b.run._2,c)))))
  }

  // with the above function we can combine the two stateful
  // computations with a function that throws away the Unit from sum.
  val checkForRepeatsAdjAndSum: Int ⇒ ROIRIWW[Boolean] = run2RWState(checkForRepeatsAdj,sum,(a:Boolean,_:Any) ⇒ a)

  // since the adjunctions compose, we can run both stateful
  // computations with a single traverse of the list. This
  // composability is something you won't find so easily with the
  // state monad by itself.
  val bothAtOnce = {
    // yay for type scala's inference :(
    implicit val adjMonad = adjOptionInt.compose[Writer[Int,?],Reader[Int,?]](adjInt).monad
    nonRepeating.traverseU(checkForRepeatsAdjAndSum).run(None).run(0).run
  }

  val repeats : Boolean = bothAtOnce._2.run._2.foldLeft(false)((x,y) ⇒ x || y)
  val sumResult : Int = bothAtOnce._1

  assert(repeats === false)  // no repeats
  assert(sumResult === 10)   // sum is 10
}
