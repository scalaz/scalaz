package scalaz.example

import scalaz.{StateT, Applicative}

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

    import scalaz.std.anyVal._, scalaz.std.list._

    def test(p: Boolean): Int = if (p) 1 else 0

    // To count words, we need to detect transitions from whitespace to non-whitespace.
    // This is tracked in a Boolean state
    def atWordEnd(c: Char) = State[Boolean, Int] {
      (inWord) =>
        val s = c != ' '
        (test(!inWord && s), s)
    }
    // To count, we traverse with a function returning 0 or 1, and sum the results
    // with Monoid[Int], packaged in a constant monoidal applicative functor.
    val Count = Applicative.monoidalApplicative[Int]

    // Compose the applicative instance for [a]State[Boolean,a] with the Count applicative
    val WordCount = StateT.stateMonad[Boolean].compose[({type λ[α] = Int})#λ](Count)

    // Fuse the three applicatives together in parallel...
    val A = Count
      .product[({type λ[α] = Int})#λ](Count)
      .product[({type λ[α] = State[Boolean, Int]})#λ](WordCount)

    // ... and execute them in a single traversal
    val ((charCount, lineCount), wordCountState) = A.traverse(text)((c: Char) => ((1, test(c == '\n')), atWordEnd(c)))
    val wordCount = wordCountState.eval(false)

    println("%d\t%d\t%d\t".format(lineCount, wordCount, charCount)) // 2	17	35
  }
}
