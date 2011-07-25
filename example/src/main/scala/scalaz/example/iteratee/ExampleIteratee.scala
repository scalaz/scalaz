package scalaz.example
package iteratee

// todo iteratee package is about to undergo significant change
object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._, iteratee._, effect._, Scalaz._


  def run {
    ((head[Int, Identity] >>== Stream(1, 2, 3)) run(_ => none)) assert_=== Some(1)
    ((length[Int, Identity] >>== Stream(10, 20, 30)) run(_ => -1)) assert_=== 3
    ((peek[Int, Identity] >>== Stream(1, 2, 3)) run(_ => none)) assert_=== Some(1)
    ((head[Int, Identity] >>== Stream()) run(_ => some(0))) assert_=== None

    ((head[Int, IO] >>== Iterator(1, 2, 3)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some(1)
    ((length[Int, IO] >>== Iterator(10, 20, 30)) runT(_ => IO(-1)) unsafePerformIO) assert_=== 3
    ((peek[Int, IO] >>== Iterator(1, 2, 3)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some(1)
    ((head[Int, IO] >>== Iterator()) runT(_ => IO(Some(-1))) unsafePerformIO) assert_=== None

    import java.io._

    def r = new StringReader("file contents")

    ((head[IoExceptionOr[Char], IO] >>== r) map (_ flatMap (_.toOption)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some('f')
    ((length[IoExceptionOr[Char], IO] >>== r) runT(_ => IO(-1)) unsafePerformIO) assert_=== 13
    ((peek[IoExceptionOr[Char], IO] >>== r) map (_ flatMap (_.toOption)) runT(_ => IO(none)) unsafePerformIO) assert_=== Some('f')
    ((head[IoExceptionOr[Char], IO] >>== new StringReader("")) map (_ flatMap (_.toOption)) runT(_ => IO(Some('z'))) unsafePerformIO) assert_=== None

    // As a monad
    val m1 = head[Int, Identity] flatMap ((b:Option[Int]) => head[Int, Identity] map (b2 => (b <|*|> b2)))
    ((m1 >>== Stream(1,2,3)) run(_ => none)) assert_=== Some(1 -> 2)

    val coli = collect[Int, List] <~ takeWhile(_ <= 5) 
    ((coli >>== (1 to 10).toStream) run(_ => List())) assert_=== (1 to 5).toList

    val colc = collect[IoExceptionOr[Char], List].up[IO] <~ takeWhile(_.fold(_ => false, _ != ' '))
    ((colc >>== r) map(_ flatMap (_.toOption)) runT(_ => IO(List())) unsafePerformIO) assert_=== List('f', 'i', 'l', 'e')
  }
}
