package scalaz.example

import annotation.unchecked.uncheckedVariance

object TrampolineUsage extends App {

  import scalaz._, Scalaz._, Free._

  def runQuickSort[F[+ _], T](xs: List[T])(implicit FP: Pointed[F], FC: CoPointed[F], T: Order[T]): List[T] =
    quickSort[F, T](xs).go(f => FC.copoint(f))

  def quickSort[F[+ _] : Pointed, T: Order](xs: List[T]): Free[F, List[T]] = {
    xs match {
      case Nil =>
        return_ {
          Nil
        }
      case x :: tail =>
        val (left, right) = tail.partition(_ < x)
        suspend {
          for {
            ls <- quickSort[F, T](left)
            rs <- quickSort[F, T](right)
          } yield ls ::: (x :: rs)
        }
    }
  }

  val xs = List.fill(32)(util.Random.nextInt())

  {
    // Trampoline is Free[Function0, A].

    // use the heap
    val sorted = runQuickSort[Function0, Int](xs)
    println(sorted)

    val (steps, sorted1) = quickSort[Function0, Int](xs).foldRun(0)((i, f) => (i + 1, f()))
    println("sort using heap took %d steps".format(steps))
  }

  {
    // Use the stack.
    val sorted = runQuickSort[Id, Int](xs)
    println(sorted)
  }
}