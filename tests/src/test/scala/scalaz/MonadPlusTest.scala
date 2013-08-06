package scalaz

import std.AllInstances._
import std.option.{some, none}

class MonadPlusTest extends Spec {

  "unite" in {
    MonadPlus[List].unite(List(some(1), none[Int], some(2))) must be_===(List(1, 2))
  }

  "uniteU" in {
    MonadPlus[List].uniteU(List(\/.right(1), \/.left("a"), \/.right(2))) must be_===(List(1, 2))
  }

  "separate" in {
    import \&/._
    import syntax.monadPlus._

    List(\/.right(1), \/.left("a"), \/.right(2)).separate must be_===(List("a") -> List(1, 2))

    List(1 -> "a", 2 -> "b").separate must be_===(List(1, 2) -> List("a", "b"))

    Vector[Int \&/ String](This(1), Both(3, "a"), That("b")).separate must be_===(Vector(3) -> Vector("a"))

    Stream(Success(1), Failure("a"), Success(2)).separate must be_===(Stream("a") -> Stream(1, 2))
  }

  "filter" in {
    MonadPlus[List].filter(List(1, 2, 3))(_ % 2 == 0) must be_===(List(2))
  }
}
