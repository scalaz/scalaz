package scalaz

import std.list._, std.tuple._, std.anyVal._

object ZipTest extends SpecLite {

  "Zip.ap laziness" ! {
    val F = Zip[List].ap
    def undefined[A]: List[A] = sys.error("")
    F.apply2(nil[Int], undefined[Int])(Tuple2.apply) must_=== Nil
    F.apply3(nil[Int], undefined[Int], List(1))(Tuple3.apply) must_=== Nil
    F.apply3(List(1), nil[Int], undefined[Int])(Tuple3.apply) must_=== Nil

    F.tuple2(nil[Int], undefined[Int]) must_=== Nil
    F.tuple3(nil[Int], undefined[Int], undefined[Int]) must_=== Nil
    F.tuple3(List(1), nil[Int], undefined[Int]) must_=== Nil
  }

}


