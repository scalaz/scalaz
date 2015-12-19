package scalaz.example

import scalaz.{Monoid, StateT}


/**
 * Character/Line/Word Count from "The Essense of the Iterator Pattern".
 *
 * @see [[http://www.comlab.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf]]
 */
object WordCount {
  def main(args: Array[String]) = {
    wordCount
  }

  def wordCount {
    import scalaz.State

    val text = "the cat in the hat\n sat on the mat\n".toList

    import scalaz.std.anyVal._, scalaz.std.list._, scalaz.std.boolean.test, scalaz.std.int.heaviside

    // To count words, we need to detect transitions from whitespace to non-whitespace.
    // atWordStart_{i} = heaviside( test(isSpace(c_{i}) - test(isSpace(c_{i-1})) )
    def atWordStart(c: Char): State[Boolean, Int] = State { (prev: Boolean) =>
      val cur = c != ' '
      (cur, test(cur && !prev))
    }

    // To count, we traverse with a function returning 0 or 1, and sum the results
    // with Monoid[Int], packaged in a constant monoidal applicative functor.
    val Count = Monoid[Int].applicative

    // Compose the applicative instance for [a]State[Boolean,a] with the Count applicative
    val WordCount = StateT.stateMonad[Boolean].compose[λ[α=>Int]](Count)

    // Fuse the three applicatives together in parallel...
    val A = Count
      .product[λ[α=>Int]](Count)
      .product[λ[α=>State[Boolean,Int]]](WordCount)

    // ... and execute them in a single traversal
    val ((charCount, lineCount), wordCountState) = A.traverse(text)((c: Char) => ((1, test(c == '\n')), atWordStart(c)))
    val wordCount = wordCountState.eval(false)

    println("%d\t%d\t%d\t".format(lineCount, wordCount, charCount)) // 2	9	35
  }
}
