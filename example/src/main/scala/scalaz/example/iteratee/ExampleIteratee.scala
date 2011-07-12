package scalaz.example
package iteratee

// todo iteratee package is about to undergo significant change
object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._, iteratee._, effect._, Scalaz._


  def run {
    (head[Int] enumerate Stream(1, 2, 3) run) assert_=== Some(1)
    (length[Int] enumerate Stream(10, 20, 30) run) assert_=== 3
    (peek[Int] enumerate Stream(1, 2, 3) run) assert_=== Some(1)
    (head[Int] enumerate Stream() run) assert_=== None

    (head[Int] enumerateUp Iterator(1, 2, 3) flatMap (_.runT) unsafePerformIO) assert_=== Some(1)
    (length[Int] enumerateUp Iterator(10, 20, 30) flatMap (_.runT) unsafePerformIO) assert_=== 3
    (peek[Int] enumerateUp Iterator(1, 2, 3) flatMap (_.runT) unsafePerformIO) assert_=== Some(1)
    (head[Int] enumerateUp Iterator() flatMap (_.runT) unsafePerformIO) assert_=== None

    import java.io._

    def r = new StringReader("file contents")


    (head[IoExceptionOr[Char]] enumerateUp r map (_ map (_ flatMap (_.toOption))) flatMap (_.runT) unsafePerformIO) assert_=== Some('f')
    (length[IoExceptionOr[Char]] enumerateUp r flatMap (_.runT) unsafePerformIO) assert_=== 13
    (peek[IoExceptionOr[Char]] enumerateUp r map (_ map (_ flatMap (_.toOption))) flatMap (_.runT) unsafePerformIO) assert_=== Some('f')
    (head[IoExceptionOr[Char]] enumerateUp (new StringReader("")) map (_ map (_ flatMap (_.toOption))) flatMap (_.runT) unsafePerformIO) assert_=== None

    // As a monad
    val m1 = head[Int] >>= ((b:Option[Int]) => head[Int] map (b2 => (b <|*|> b2)))
    (m1 enumerate Stream(1,2,3) run) assert_=== Some(1 -> 2)

    val coli = takeWhile[Int, Identity, List[Int]](_ <= 5) apply collect[Int, List]
    ((coli enumerate (1 to 10).toStream run) run) assert_=== (1 to 5).toList
    (coli flatMap { i: Iteratee[Int, List[Int]] => length[Int] map (l => i.run :+ l) } enumerate (1 to 17).toStream run) assert_=== (1 to 5).toList :+ 12

    val colc = takeWhile[IoExceptionOr[Char], IO, List[IoExceptionOr[Char]]](_.fold(_ => false, _ != ' ')) apply collect[IoExceptionOr[Char], List].up[IO]
    (colc enumerateT r map { _ map { _ map { _.map(_.toOption).sequence } } } flatMap { _.runT } flatMap { _.runT } unsafePerformIO) assert_=== Some(List('f', 'i', 'l', 'e'))
  }
}
