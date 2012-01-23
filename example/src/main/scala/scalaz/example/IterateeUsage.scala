package scalaz.example

object IterateeUsage extends App {
  import scalaz._, Scalaz._
  import iteratee._, Iteratee._
  import effect._, IO._

  val stream123 = enumStream[Unit, Int, Id](Stream(1, 2, 3))

  ((head[Unit, Int, Id]   &= stream123).runOrZero) assert_=== Some(1)
  ((length[Unit, Int, Id] &= stream123) apply(_ => -1)) assert_=== 3
  ((peek[Unit, Int, Id]   &= stream123).runOrZero) assert_=== Some(1)
  ((head[Unit, Int, Id]   &= enumStream(Stream())) apply(_ => some(0))) assert_=== None

  val iter123 = enumIterator[Unit, Int](Iterator(1, 2, 3))

  ((head[Unit, Int, IO]   &= iter123).runOrZero unsafePerformIO) assert_=== Some(1)
  ((length[Unit, Int, IO] &= iter123) apply(_ => IO(-1)) unsafePerformIO) assert_=== 3
  ((peek[Unit, Int, IO]   &= iter123).runOrZero unsafePerformIO) assert_=== Some(1)
  ((head[Unit, Int, IO]   &= enumIterator(Iterator())) apply(_ => IO(Some(-1))) unsafePerformIO) assert_=== None

  val stream1_10 = enumStream[Unit, Int, Id](1 to 10 toStream)

  (take[Unit, Int, List](3) &= stream1_10).runOrZero assert_=== List(1, 2, 3)
  (takeWhile[Unit, Int, List](_ <= 5) &= stream1_10).runOrZero assert_=== (1 to 5).toList
  (takeUntil[Unit, Int, List](_ >  5) &= stream1_10).runOrZero assert_=== (1 to 5).toList

  val readLn = takeWhile[Unit, Char, List](_ != '\n') flatMap (ln => drop[Unit, Char, Id](1).map(_ => ln))
  (collect[Unit, List[Char], List] %= readLn.sequenceI &= enumStream("Iteratees\nare\ncomposable".toStream)).runOrZero assert_=== List("Iteratees".toList, "are".toList, "composable".toList)

  (collect[Unit, List[Int], List] %= splitOn(_ % 3 != 0) &= stream1_10).runOrZero assert_=== List(List(1, 2), List(4, 5), List(7, 8), List(10))

  (collect[Unit, Int, List] %= map((_:String).toInt) &= enumStream(Stream("1", "2", "3"))).runOrZero assert_=== List(1, 2, 3)
  (collect[Unit, Int, List] %= filter((_:Int) % 2 == 0) &= stream1_10).runOrZero assert_=== List(2, 4, 6, 8, 10)

  (collect[Unit, List[Int], List] %= group(3) &= enumStream((1 to 9).toStream)).runOrZero assert_=== List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

  import java.io._

  def r = enumReader[Unit](new StringReader("file contents"))

  ((head[Unit, IoExceptionOr[Char], IO]   &= r) map (_ flatMap (_.toOption)) apply(_ => IO(none)) unsafePerformIO) assert_=== Some('f')
  ((length[Unit, IoExceptionOr[Char], IO] &= r) apply(_ => IO(-1)) unsafePerformIO) assert_=== 13
  ((peek[Unit, IoExceptionOr[Char], IO]   &= r) map (_ flatMap (_.toOption)) apply(_ => IO(none)) unsafePerformIO) assert_=== Some('f')
  ((head[Unit, IoExceptionOr[Char], IO]   &= enumReader(new StringReader(""))) map (_ flatMap (_.toOption)) apply(_ => IO(Some('z'))) unsafePerformIO) assert_=== None

  // As a monad
  val m1 = head[Unit, Int, Id] flatMap (b => head[Unit, Int, Id] map (b2 => (b pair b2)))
  (m1 &= stream123).runOrZero assert_=== Some(1 -> 2)

  // As a monad using for comprehension (same as 'm1' example above)
  val m2 = for{
    b <- head[Unit, Int, Id]
    b2 <- head[Unit, Int, Id]
  } yield b pair b2
  (m2 &= stream123).apply(_ => none) assert_=== Some(1 -> 2)

  val colc = takeWhile[Unit, IoExceptionOr[Char], List](_.fold(_ => false, _ != ' ')).up[IO]
  ((colc &= r).map(_ flatMap (_.toOption)).runOrZero unsafePerformIO) assert_=== List('f', 'i', 'l', 'e')

  val take10And5ThenHead = take[Unit, Int, List](10) zip take[Unit, Int, List](5) flatMap (ab => head[Unit, Int, Id] map (h => (ab, h)))
  (take10And5ThenHead &= enumStream((1 to 20).toStream)).runOrZero assert_=== (((1 to 10).toList, (1 to 5).toList), Some(11))
}
