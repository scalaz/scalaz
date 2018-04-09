// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

package object console {

  /**
   * Prints text to the console.
   */
  def putStr(line: String): IO[Unit] = IO.sync(scala.Console.print(line))

  /**
   * Prints a line of text to the console, including a newline character.
   */
  def putStrLn(line: String): IO[Unit] = IO.sync(scala.Console.println(line))

  /**
   * Retrieves a line of input from the console.
   */
  val getStrLn: IO[String] = IO.sync(scala.io.StdIn.readLine())
}
