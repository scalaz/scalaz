package scalaz.effect

object ScalazIOArray {

  def bubbleSort[A](lessThanEqual0: (A, A) => Boolean)(array: Array[A]): IO[Void, Unit] = {

    type IndexValue   = (Int, A)
    type IJIndexValue = (IndexValue, IndexValue)

    val lessThanEqual =
      KleisliIO.lift[Void, IJIndexValue, Boolean] {
        case ((_, ia), (_, ja)) => lessThanEqual0(ia, ja)
      }

    val extractIJAndIncrementJ = KleisliIO.lift[Void, IJIndexValue, (Int, Int)] {
      case ((i, _), (j, _)) => (i, j + 1)
    }

    val extractIAndIncrementI = KleisliIO.lift[Void, (Int, Int), Int](_._1 + 1)

    val innerLoopStart = KleisliIO.lift[Void, Int, (Int, Int)]((i: Int) => (i, i + 1))

    val outerLoopCheck: KleisliIO[Void, Int, Boolean] =
      KleisliIO.lift((i: Int) => i < array.length - 1)

    val innerLoopCheck: KleisliIO[Void, (Int, Int), Boolean] =
      KleisliIO.lift { case (_, j) => j < array.length }

    val extractIJIndexValue: KleisliIO[Void, (Int, Int), IJIndexValue] =
      KleisliIO.impureVoid {
        case (i, j) => ((i, array(i)), (j, array(j)))
      }

    val swapIJ: KleisliIO[Void, IJIndexValue, IJIndexValue] =
      KleisliIO.impureVoid {
        case v @ ((i, ia), (j, ja)) =>
          array.update(i, ja)
          array.update(j, ia)

          v
      }

    val sort = KleisliIO
      .whileDo(outerLoopCheck)(
        innerLoopStart >>>
          KleisliIO.whileDo(innerLoopCheck)(
            extractIJIndexValue >>>
            KleisliIO.ifThenElse(lessThanEqual)(KleisliIO.identity)(swapIJ) >>>
            extractIJAndIncrementJ
          ) >>>
          extractIAndIncrementI
      )
    sort(0).toUnit
  }
}

object CatsIOArray {
  import cats.effect.IO

  def bubbleSort[A](lessThanEqual0: (A, A) => Boolean)(array: Array[A]): IO[Unit] = {
    def outerLoop(i: Int): IO[Unit] =
      if (i >= array.length - 1) IO.unit else innerLoop(i, i + 1).flatMap(_ => outerLoop(i + 1))

    def innerLoop(i: Int, j: Int): IO[Unit] =
      if (j >= array.length) IO.unit
      else
        IO((array(i), array(j))).flatMap {
          case (ia, ja) =>
            val maybeSwap = if (lessThanEqual0(ia, ja)) IO.unit else swapIJ(i, ia, j, ja)

            maybeSwap.flatMap(_ => innerLoop(i, j + 1))
        }

    def swapIJ(i: Int, ia: A, j: Int, ja: A): IO[Unit] =
      IO { array.update(i, ja); array.update(j, ia) }

    outerLoop(0)
  }
}
