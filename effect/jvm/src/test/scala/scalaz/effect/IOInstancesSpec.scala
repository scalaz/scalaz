package scalaz.effect

import org.specs2.Specification

class IOInstancesSpec extends Specification with RTS {

  def is = s2"""
   IO instances for:
      bifunctor                                                        $testIOBifunctor
    """

  def testIOBifunctor = {
    import scalaz._
    val rmapRes: IO[Void, Int]             = Bifunctor[IO].rmap(IO.sync[Void, String]("12"))(str => str.toInt)
    val lmapRes: IO[RuntimeException, Int] = Bifunctor[IO].lmap(rmapRes)(err => new RuntimeException("error"))
    unsafePerformIO(lmapRes) must_=== 12
  }

}
