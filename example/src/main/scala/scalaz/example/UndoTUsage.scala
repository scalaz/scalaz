package scalaz.example

import scalaz.{syntax, std}
import syntax.monad._
import std.option._
import scalaz.undo.UndoT
import scalaz.undo.UndoT._

object UndoTUsage extends App {
  // TODO: Omitting the type parameters on hput leads to a compiler infinite loop
  // if UndoT.undoTMonadState is imported.

  val result: UndoT[Option, Int, _] =
    for {
      one           <- hput[Option, Int](1)
      two           <- hput[Option, Int](2)
      three         <- hput[Option, Int](3)
      twoAgain      <- undo[Option, Int]
      four          <- hput[Option, Int](4)
      twoAgainAgain <- undo[Option, Int]
      fourAgain     <- redo[Option, Int]
    } yield ()

  // This should print 'Some(4)'
  assert(result.exec(1) == Some(4))
  println("success")

}
