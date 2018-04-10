// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

package object console {

  /**
   * Prints text to the console.
   */
  def putStr[E](line: String): IO[E, Unit] = IO.sync(scala.Console.print(line))

  /**
   * Prints a line of text to the console, including a newline character.
   */
  def putStrLn[E](line: String): IO[E, Unit] = IO.sync(scala.Console.println(line))

  /**
   * Retrieves a line of input from the console.
   */
  def getStrLn[E]: IO[E, String] = IO.sync(scala.io.StdIn.readLine())
}
