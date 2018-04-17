// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import java.io.IOException

package object console {
  private val ioException: PartialFunction[Throwable, IOException] = {
    case e: IOException => e
  }

  /**
   * Prints text to the console.
   */
  def putStr(line: String): IO[IOException, Unit] =
    IO.syncCatch(scala.Console.print(line))(ioException)

  /**
   * Prints a line of text to the console, including a newline character.
   */
  def putStrLn(line: String): IO[IOException, Unit] =
    IO.syncCatch(scala.Console.println(line))(ioException)

  /**
   * Retrieves a line of input from the console.
   */
  def getStrLn: IO[IOException, String] =
    IO.syncCatch(scala.io.StdIn.readLine())(ioException)
}
