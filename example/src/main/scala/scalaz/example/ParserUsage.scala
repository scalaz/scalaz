package scalaz.example

import util.parsing.combinator.RegexParsers
import collection.immutable.List
import scalaz._

object ParserUsage extends App {

  object testParser extends RegexParsers {
    val parsers: List[Parser[List[String]]] = List(1, 2, 3).map(repN(_, """\d+""".r))

    def apply(s: String): ParseResult[List[List[String]]] = {
      import syntax.applicative._ // for _.sequence
      import std.List._ // for Traverse[List]
      import std.util.parsing.combinator.Parser.parserMonad

      implicit val M: Monad[Parser] = parserMonad(testParser)
      val sequence: Parser[List[List[String]]] = parsers.sequence
      parseAll(sequence, s)
    }
  }

  // http://stackoverflow.com/questions/7785762/writing-type-class-instances-for-nested-classes-in-scala
  val result: List[List[String]] = testParser("1 2 2 3 3 3").getOrElse(Nil)
  assert(result == List(List("1"), List("2", "2"), List("3", "3", "3")))
}