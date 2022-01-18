package scalaz

object LeibnizTest extends SpecLite {
  import Leibniz.refl, std.function._

  // That these compile.
  val x = List(1) map refl[Int].onF(a => a)
  val y = refl[Int].onCov(\/.right[Int,Int](42))
  val z = refl[Int].onContra((i:Int) => i)
  (x, y, z): (List[Int], Int \/ Int, Int => Int)
}
