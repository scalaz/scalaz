package scalaz.example

object TrampolineUsage extends App {

  import scalaz._, Scalaz._, Free._

  def quickSort[F[+ _] : Pointed, T: Order](xs: List[T]): Free[F, List[T]] = {
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

  def runQuickSort[F[+ _] : Pointed : Copointed, T: Order](xs: List[T]): List[T] =
    quickSort[F, T](xs).go(f => Copointed[F].copoint(f))

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

  import scalaz.concurrent._
  import annotation.unchecked.uncheckedVariance
  type PromiseCov[+A] = Promise[A @uncheckedVariance]

  {
    // Run in parallel.
    val sorted = runQuickSort[PromiseCov, Int](xs)
    println(sorted)
  }


  /**Run using `F1` as a binding for lists longer than `threshold`, and `F2` otherwise. */
  def quickSort2[F[+ _] : Pointed, F2[+ _] : Pointed, T: Order](xs: List[T], nat: F2 ~> F, threshold: Int): Free[F, List[T]] = {
    def qs(as: List[T]): Free[F, List[T]] =
      if (as.lengthCompare(threshold) < 0) {
        val free: Free[F2, List[T]] = quickSort2[F2, F2, T](as, NaturalTransformation.refl[F2], threshold)
        free.mapSuspension(nat)
      } else quickSort2[F, F2, T](as, nat, threshold)

    xs match {
      case Nil =>
        return_ {
          Nil
        }
      case x :: tail =>
        suspend {
          val (left, right) = tail.partition(_ < x)
          for {
            ls <- qs(left)
            rs <- qs(right)
          } yield ls ::: (x :: rs)
        }
    }
  }

  def runQuickSort2[F[+ _] : Pointed : Copointed, F2[+ _] : Pointed, T: Order](xs: List[T], nat: F2 ~> F, threshold: Int): List[T] =
    quickSort2[F, F2, T](xs, nat, threshold).go(f => Copointed[F].copoint(f))


  {
    // mixed binding
    val promise2Id = new (Id ~> PromiseCov) { def apply[A](a: A) = Promise(a)}

    // run in parallel for lists larger then 8 elements, and on the stack for lists smaller.
    val sorted = runQuickSort2[PromiseCov, Id, Int](xs, promise2Id, 8)
    println(sorted)
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
