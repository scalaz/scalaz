package scalaz.example

import scalaz.undo.Undo._
import scalaz.syntax.Syntax.monad._
import scalaz.std.Option._
import scalaz.undo.UndoT

object UndoTUsage extends App {

  val result: UndoT[Int, Option, _] =
    for {
      one           <- hput(1)
      two           <- hput(2)
      three         <- hput(3)
      twoAgain      <- undo[Int, Option]
      four          <- hput(4)
      twoAgainAgain <- undo[Int, Option]
      fourAgain     <- redo[Int, Option]
    } yield ()

  // This should print 'Some(4)'
  assert(result !! 1 == Some(4))
  println("success")

}
