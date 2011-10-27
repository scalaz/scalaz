package scalaz.example

import scalaz.{syntax, std}
import syntax.monad._
import std.option._
import scalaz.undo.UndoT
import scalaz.undo.UndoT._

object UndoTUsage extends App {
  // TODO: Omitting the type parameters on hput leads to a compiler infinite loop
  // if UndoT.undoTMonadState is imported.

  val result: UndoT[Int, Option, _] =
    for {
      one           <- hput[Int, Option](1)
      two           <- hput[Int, Option](2)
      three         <- hput[Int, Option](3)
      twoAgain      <- undo[Int, Option]
      four          <- hput[Int, Option](4)
      twoAgainAgain <- undo[Int, Option]
      fourAgain     <- redo[Int, Option]
    } yield ()

  // This should print 'Some(4)'
  assert(result.exec(1) == Some(4))
  println("success")

}
