package scalazdemo.io

import scalaz.io.File._

object File {
  import scala.collection.mutable.ListBuffer
  import List.flatten

  val file = "/etc/passwd"

  val demoes = List(
    // read

    // the length of the file
    ("file read (0, (n: Int, b) => n + 1)", file read (0, (n: Int, b) => n + 1)),

    // the contents of the file in upper case
    ("file.read(new ListBuffer[Char], (b: ListBuffer[Char], x) => { b + x.toChar.toUpperCase; b}).mkString",
            file.read(new ListBuffer[Char], (b: ListBuffer[Char], x) => { b + x.toChar.toUpperCase; b}).mkString),

    // readLines

    // the number of lines in the file
    ("file.readLines[Unit, Int]((), (x, y) => x, 0, (n, x) => n + 1)",
            file.readLines[Unit, Int]((), (x, y) => x, 0, (n, x) => n + 1)),

    // the contents of the file in upper case, with each line reversed and line separators removed
    ("flatten(file.readLines[List[Char], List[List[Char]]](Nil, (x, y) => y.toUpperCase :: x, Nil, (x, y) => y :: x)).mkString",
            flatten(file.readLines[List[Char], List[List[Char]]](Nil, (x, y) => y.toUpperCase :: x, Nil, (x, y) => y :: x)).mkString)
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => println(s + " :::: " + x) }
  }
}
