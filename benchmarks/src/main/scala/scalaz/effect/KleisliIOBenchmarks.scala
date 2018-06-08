package scalaz.effect

import scala.{ Array, Boolean, Int, Unit }

import scalaz.Void

object ScalazIOArray {

  def bubbleSort[A](lessThanEqual0: (A, A) => Boolean)(array: Array[A]): IO[Void, Unit] = {

    type IndexValue   = (Int, A)
    type IJIndex      = (Int, Int)
    type IJIndexValue = (IndexValue, IndexValue)

    val lessThanEqual =
      KleisliIO.lift[Void, IJIndexValue, Boolean] {
        case ((_, ia), (_, ja)) => lessThanEqual0(ia, ja)
      }

    val extractIJAndIncrementJ = KleisliIO.lift[Void, IJIndexValue, IJIndex] {
      case ((i, _), (j, _)) => (i, j + 1)
    }

    val extractIAndIncrementI = KleisliIO.lift[Void, IJIndex, Int](_._1 + 1)

    val innerLoopStart = KleisliIO.lift[Void, Int, IJIndex]((i: Int) => (i, i + 1))

    val outerLoopCheck: KleisliIO[Void, Int, Boolean] =
      KleisliIO.lift((i: Int) => i < array.length - 1)

    val innerLoopCheck: KleisliIO[Void, IJIndex, Boolean] =
      KleisliIO.lift { case (_, j) => j < array.length }

    val extractIJIndexValue: KleisliIO[Void, IJIndex, IJIndexValue] =
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
              KleisliIO.ifNotThen(lessThanEqual)(swapIJ) >>>
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

object MonixIOArray {
  import monix.eval.Task

  def bubbleSort[A](lessThanEqual0: (A, A) => Boolean)(array: Array[A]): Task[Unit] = {
    def outerLoop(i: Int): Task[Unit] =
      if (i >= array.length - 1) Task.unit else innerLoop(i, i + 1).flatMap(_ => outerLoop(i + 1))

    def innerLoop(i: Int, j: Int): Task[Unit] =
      if (j >= array.length) Task.unit
      else
        Task.eval((array(i), array(j))).flatMap {
          case (ia, ja) =>
            val maybeSwap = if (lessThanEqual0(ia, ja)) Task.unit else swapIJ(i, ia, j, ja)

            maybeSwap.flatMap(_ => innerLoop(i, j + 1))
        }

    def swapIJ(i: Int, ia: A, j: Int, ja: A): Task[Unit] =
      Task.eval { array.update(i, ja); array.update(j, ia) }

    outerLoop(0)
  }
}
