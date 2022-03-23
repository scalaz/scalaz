package scalaz.example

object TrampolineUsage {

  import scalaz._, Scalaz._, Free._

  def quickSort[F[_] : Applicative, T: Order](xs: List[T]): Free[F, List[T]] = {
    xs match {
      case Nil =>
        return_ {
          Nil
        }
      case x :: tail =>
        suspend {
          val (left, right) = tail.partition(_ < x)
          for {
            ls <- quickSort[F, T](left)
            rs <- quickSort[F, T](right)
          } yield ls ::: (x :: rs)
        }
    }
  }

  def runQuickSort[F[_] : Applicative : Comonad, T: Order](xs: List[T]): List[T] =
    quickSort[F, T](xs).go(f => Comonad[F].copoint(f))(Applicative[F])

  def main(args: Array[String]): Unit = {
    val xs = List.fill(32)(util.Random.nextInt())

    {
      // Trampoline is Free[Function0, A].

      // use the heap
      val sorted = runQuickSort[Function0, Int](xs)
      println(sorted)

      val step = new ~>[λ[α => (Int, Function0[α])], (Int, *)] {
        def apply[A](x: (Int, Function0[A])) = {
          (x._1 + 1, x._2.apply())
        }
      }

      val (steps, sorted1) = quickSort[Function0, Int](xs).foldRun(0)(step)
      println("sort using heap took %d steps".format(steps))
    }

    {
      // Use the stack.
      val sorted = runQuickSort[Id, Int](xs)
      println(sorted)
    }

  }

  // Ackermann function. Blows the stack for very small inputs.
  def ack(m: Int, n: Int): Int =
    if (m <= 0)
      n + 1
    else if (n <= 0)
      ack(m - 1, 1)
    else ack(m - 1, ack(m, n - 1))

  // Trampolined ackermann function. Never blows the stack, even for large inputs.
  def ackermann(m: Int, n: Int): Trampoline[Int] =
    if (m <= 0)
      return_(n + 1)
    else if (n <= 0)
      suspend(ackermann(m - 1, 1))
    else for {
      a <- suspend(ackermann(m, n - 1))
      b <- suspend(ackermann(m - 1, a))
    } yield b

}
