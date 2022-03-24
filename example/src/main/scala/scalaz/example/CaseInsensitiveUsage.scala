package scalaz.example

import scalaz.{CaseInsensitive => CI}

object CaseInsensitiveUsage {

  def maps(): Unit = {
    val nums: Map[CI[String], Int] = Map(CI("ONE") -> 1, CI("Two") -> 2)

    assert(nums(CI("one")) == 1)
    assert(nums(CI("TWO")) == 2)
  }

  def sets(): Unit = {
    val nums: Set[CI[String]] = Set(CI("ONE"), CI("Two"))

    assert(nums(CI("one")))
    assert(nums(CI("TWO")))
  }

  def main(args: Array[String]): Unit = {
    maps()
    sets()
  }
}
