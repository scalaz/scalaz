package scalaz.example

object IterateeUsage extends App {
  import scalaz._, Scalaz._, MonadPartialOrder._
  import iteratee._, Iteratee._
  import effect._

  val stream123 = enumStream[Int, Id](Stream(1, 2, 3))

  ((head[Int, Id]   &= stream123).run) assert_=== Some(1)
  ((length[Int, Id] &= stream123).run) assert_=== 3
  ((peek[Int, Id]   &= stream123).run) assert_=== Some(1)
  ((head[Int, Id]   &= enumStream(Stream())).run) assert_=== None

  def iter123 = enumIterator[Int, IO](Iterator(1, 2, 3))

  ((head[Int, IO]   &= iter123).run unsafePerformIO()) assert_=== Some(1)
  ((length[Int, IO] &= iter123).run unsafePerformIO()) assert_=== 3
  ((peek[Int, IO]   &= iter123).run unsafePerformIO()) assert_=== Some(1)
  ((head[Int, IO]   &= enumIterator[Int, IO](Iterator())).run unsafePerformIO()) assert_=== None

  val stream1_10 = enumStream[Int, Id]((1 to 10).toStream)

  (take[Int, List](3) &= stream1_10).run assert_=== List(1, 2, 3)
  (takeWhile[Int, List](_ <= 5) &= stream1_10).run assert_=== (1 to 5).toList
  (takeUntil[Int, List](_ >  5) &= stream1_10).run assert_=== (1 to 5).toList

  val readLn = takeWhile[Char, List](_ != '\n') flatMap (ln => drop[Char, Id](1).map(_ => ln))
  (collect[List[Char], List] %= readLn.sequenceI &= enumStream("Iteratees\nare\ncomposable".toStream)).run assert_=== List("Iteratees".toList, "are".toList, "composable".toList)

  (collect[List[Int], List] %= splitOn(_ % 3 != 0) &= stream1_10).run assert_=== List(List(1, 2), List(4, 5), List(7, 8), List(10))

  (collect[Int, List] %= map((_:String).toInt) &= enumStream(Stream("1", "2", "3"))).run assert_=== List(1, 2, 3)
  (collect[Int, List] %= filter((_:Int) % 2 == 0) &= stream1_10).run assert_=== List(2, 4, 6, 8, 10)

  (collect[List[Int], List] %= group(3) &= enumStream((1 to 9).toStream)).run assert_=== List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

  import java.io._

  def r = enumReader[IO](new StringReader("file contents"))

  ((head[IoExceptionOr[Char], IO] &= r).map(_ flatMap (_.toOption)).run.unsafePerformIO()) assert_=== Some('f')
  ((length[ IoExceptionOr[Char], IO] &= r).run.unsafePerformIO()) assert_=== 13
  ((peek[IoExceptionOr[Char], IO]   &= r).map(_ flatMap (_.toOption)).run.unsafePerformIO()) assert_=== Some('f')
  ((head[IoExceptionOr[Char], IO]  &= enumReader[IO](new StringReader(""))).map(_ flatMap (_.toOption)).run unsafePerformIO()) assert_=== None

  // As a monad
  val m1 = head[Int, Id] flatMap (b => head[Int, Id] map (b2 => (b tuple b2)))
  (m1 &= stream123).run assert_=== Some(1 -> 2)

  // As a monad using for comprehension (same as 'm1' example above)
  val m2 = for{
    b <- head[Int, Id]
    b2 <- head[Int, Id]
  } yield b tuple b2
  (m2 &= stream123).run assert_=== Some(1 -> 2)

  val colc = takeWhile[IoExceptionOr[Char], List](_.fold(_ => false, _ != ' ')).up[IO]
  ((colc &= r).map(_ flatMap (_.toOption)).run unsafePerformIO()) assert_=== List('f', 'i', 'l', 'e')

  val take10And5ThenHead = take[Int, List](10) zip take[Int, List](5) flatMap (ab => head[Int, Id] map (h => (ab, h)))
  (take10And5ThenHead &= enumStream((1 to 20).toStream)).run assert_=== (((1 to 10).toList, (1 to 5).toList), Some(11))
}
