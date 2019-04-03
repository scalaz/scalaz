package scalaz

import std.AllInstances._

object FreeTestJVM extends SpecLite {
  "foldMapRec is stack safe" ! {
    val n = 1000000
    trait FTestApi[A]
    case class TB(i: Int) extends FTestApi[Int]

    def a(i: Int): Free[FTestApi, Int] = for {
      j <- Free.liftF(TB(i))
      z <- if (j < n) a(j) else Free.pure[FTestApi, Int](j)
    } yield z

    val runner = new (FTestApi ~> Id.Id) {
      def apply[A](fa: FTestApi[A]) = fa match {
        case TB(i) => i + 1
      }
    }

    a(0).foldMapRec(runner) must_=== n
  }
}
