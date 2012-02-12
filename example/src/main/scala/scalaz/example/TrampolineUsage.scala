package scalaz.example

object TrampolineUsage extends App {

  import scalaz._, Scalaz._, Free._

  def quickSort[T: Order](xs: List[T]): Trampoline[List[T]] = {
    assert(Thread.currentThread().getStackTrace.count(_.getMethodName == "quickSort") == 1)
    xs match {
      case Nil =>
        return_ {
          Nil
        }
      case x :: tail =>
        val (left, right) = tail.partition(_ < x)
        suspend {
          for {
            ls <- quickSort(left)
            rs <- quickSort(right)
          } yield ls ::: (x :: rs)
        }
    }
  }

  val xs = List.fill(32)(util.Random.nextInt())
  val sorted = quickSort(xs).run
  println(sorted)

  val (steps, sorted1) = quickSort(xs).foldRun(0)((i, f) => (i + 1, f()))
  println("sort took %d steps".format(steps))
}