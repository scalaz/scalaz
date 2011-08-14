package scalaz.example
package iteratee

// todo iteratee package is about to undergo significant change
object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._, iteratee._, effect._, Scalaz._

  def run {
    ((head[Unit, Int, Identity] >>== Stream(1, 2, 3)) run(_ => none)) assert_=== Some(1)
    ((length[Unit, Int, Identity] >>== Stream(10, 20, 30)) run(_ => -1)) assert_=== 3
    ((peek[Unit, Int, Identity] >>== Stream(1, 2, 3)) run(_ => none)) assert_=== Some(1)
    ((head[Unit, Int, Identity] >>== Stream()) run(_ => some(0))) assert_=== None

    ((head[Unit, Int, IO] >>== Iterator(1, 2, 3)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some(1)
    ((length[Unit, Int, IO] >>== Iterator(10, 20, 30)) runT(_ => IO(-1)) unsafePerformIO) assert_=== 3
    ((peek[Unit, Int, IO] >>== Iterator(1, 2, 3)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some(1)
    ((head[Unit, Int, IO] >>== Iterator()) runT(_ => IO(Some(-1))) unsafePerformIO) assert_=== None

    ((take[Unit, Int, List](3) >>== (1 to 10).toStream) run(_ => List())) assert_=== List(1, 2, 3)
    ((takeWhile[Unit, Int, List](_ <= 5) >>== (1 to 10).toStream) run(_ => List())) assert_=== (1 to 5).toList
    ((takeUntil[Unit, Int, List](_ > 5) >>== (1 to 10).toStream) run(_ => List())) assert_=== (1 to 5).toList

    val readLn = takeWhile[Unit, Char, List](_ != '\n') flatMap (ln => drop[Unit, Char, Identity](1).map(_ => ln))
    ((collect[Unit, List[Char], List] %= readLn.sequenceI[List[List[Char]]] >>== "Iteratees\nare\ncomposable".toStream) run(_ => List())) assert_=== List("Iteratees".toList, "are".toList, "composable".toList)

    ((collect[Unit, List[Int], List] %= splitOn(_ % 3 != 0) >>== (1 to 10).toStream) run(_ => List())) assert_=== List(List(1, 2), List(4, 5), List(7, 8), List(10))
    
    ((collect[Unit, Int, List] %= map((_:String).toInt) >>== Stream("1", "2", "3")) run(_ => List())) assert_=== List(1, 2, 3)
    ((collect[Unit, Int, List] %= filter((_:Int) % 2 == 0) >>== (1 to 10).toStream) run(_ => List())) assert_=== List(2, 4, 6, 8, 10)
    
    ((collect[Unit, List[Int], List] %= group(3) >>== (1 to 9).toStream) run(_ => List())) assert_=== List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    
    import java.io._

    def r = new StringReader("file contents")

    ((head[Unit, IoExceptionOr[Char], IO] >>== r) map (_ flatMap (_.toOption)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some('f')
    ((length[Unit, IoExceptionOr[Char], IO] >>== r) runT(_ => IO(-1)) unsafePerformIO) assert_=== 13
    ((peek[Unit, IoExceptionOr[Char], IO] >>== r) map (_ flatMap (_.toOption)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some('f')
    ((head[Unit, IoExceptionOr[Char], IO] >>== new StringReader("")) map (_ flatMap (_.toOption)) runT(_ => IO(Some('z'))) unsafePerformIO) assert_=== None

    // As a monad
    val m1 = head[Unit, Int, Identity] flatMap ((b:Option[Int]) => head[Unit, Int, Identity] map (b2 => (b <|*|> b2)))
    ((m1 >>== Stream(1,2,3)) run(_ => none)) assert_=== Some(1 -> 2)

    val colc = takeWhile[Unit, IoExceptionOr[Char], List](_.fold(_ => false, _ != ' ')).up[IO]
    ((colc >>== r) map(_ flatMap (_.toOption)) runT(_ => IO(List())) unsafePerformIO) assert_=== List('f', 'i', 'l', 'e')    
  }
}
