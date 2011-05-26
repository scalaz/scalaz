package scalaz.example

import scalaz._
import Scalaz._
import collection.immutable.List

object WordCount {
  def main(args: Array[String]) = {
    wordCount
  }

  /**
   * Character/Line/Word Count from "The Essense of the Iterator Pattern".
   *
   * http://www.comlab.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
   *
   * This is an experiment to discover which parts of this paper can be brought to Scalaz.
   */
  def wordCount {
    def liftC[A, B](f: A => B): A => Const[B, A] = {a: A => Const.const(f(a))}
    val charCountBody: Char => Const[Int, Char] = liftC(Function.const(1))
    def charCount(text: List[Char]): Const[Int, List[Char]] = text.traverse[({type λ[α]=Const[Int, α]})#λ, Char](charCountBody)
    def test(p: Boolean): Int = if (p) 1 else 0
    val lineCountBody: Char => Const[Int, Char] = liftC {c: Char => test(c == '\n')}
    def lineCount(text: List[Char]): Const[Int, List[Char]] = text.traverse[({type λ[α]=Const[Int, α]})#λ, Char](lineCountBody)
    val text = "the cat in the hat\n sat on the mat\n".toList

    (charCount(text): Int, lineCount(text): Int) assert_=== (35, 2)

    val wordCountLineCountBody = (a: Char) => (charCountBody(a), lineCountBody(a))

    def wordCountLineCount(text: List[Char]) = {
      val result = text.traverse[({type λ[α]=(Const[Int, α], Const[Int, α])})#λ, Char](wordCountLineCountBody)(implicitly[Applicative[({type λ[α]=Const[Int, α]})#λ]].**[({type λ[α]=Const[Int, α]})#λ](implicitly[Applicative[({type λ[α]=Const[Int, α]})#λ]]), implicitly )
      (result._1.value, result._2.value)
    }

    wordCountLineCount(text) assert_=== (35, 2)
  }
}
