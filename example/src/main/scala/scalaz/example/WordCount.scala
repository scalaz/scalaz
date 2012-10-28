package scalaz.example

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
    import scalaz.typelevel.{AppFunc, AppFuncU, HList, HNil},
      scalaz.typelevel.syntax.hlist._,
      scalaz.State._, scalaz.std.anyVal._, scalaz.std.list._,
      scalaz.std.boolean.test, scalaz.syntax.equal._

    val text = "the cat in the hat\n sat on the mat\n".toList

    // To count characters, treat Int as monoidal applicative
    val countChar = AppFuncU { (c: Char) => 1 }

    // To count lines, treat Int as monoidal applicative
    val countLine = AppFuncU { (c: Char) => test(c === '\n') }

    // To count words, we need to detect transitions from whitespace to non-whitespace.
    val countWord = AppFuncU { (c: Char) =>
      for {
        x <- get[Boolean]
        val y = c =/= ' '
        _ <- put(y)
      } yield test(y && !x)
    } @>>> AppFuncU { (x: Int) => x }

    // Compose applicative functions in parallel
    val countAll = countChar :: countLine :: (countWord consA AppFunc.HNil)
    
    // ... and execute them in a single traversal 
    val charCount :: lineCount ::  wordCountState :: HNil = countAll traverse text
    val wordCount = wordCountState.eval(false)

    println("%d\t%d\t%d\t".format(lineCount, wordCount, charCount)) // 2	9	35
  }
}
