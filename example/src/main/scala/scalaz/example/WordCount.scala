package scalaz.example

import scalaz._
import Scalaz._
import collection.immutable.{Stream, List}

object WordCount {
  def main(args: Array[String]) = {
    wordCount
  }

  /**
   * Character/Line/Word Count from "The Essense of the Iterator Pattern".
   *
   * This is an experiment to discover which parts of this paper can be brought to Scalaz.
   */
  def wordCount {
    def liftC[A, B](f: A => B) = {a: A => Const(f(a))}
    val charCountBody: (Char) => Const[Int, Nothing] = liftC(Function.const(1))
    def charCount(text: List[Char]): Const[Int, Any] = text.traverse[({type λ[α]=Const[Int, α]})#λ, Any](charCountBody)
    def test(p: Boolean): Int = if (p) 1 else 0
    val lineCountBody: (Char) => Const[Int, Nothing] = liftC {c: Char => test(c == '\n')}
    def lineCount(text: List[Char]): Const[Int, Any] = text.traverse[({type λ[α]=Const[Int, α]})#λ, Any](lineCountBody)

    val text = "the cat in the hat\n sat on the mat".toList

    (charCount(text): Int, lineCount(text): Int).println

    import Prod._

    val wordCountLineCountBody = ⊗[({type λ[α]=Const[Int, α]})#λ, ({type λ[α]=Const[Int, α]})#λ, Char, Any](charCountBody, lineCountBody) _
    def wordCountLineCount(text: List[Char]) = {
      val result = text.traverse[({type C1[α]=Const[Int, α]; type λ[α]=Prod[C1, C1, α]})#λ, Any](wordCountLineCountBody)(
        ProdApplicative[({type λ[α]=Const[Int, α]})#λ, ({type λ[α]=Const[Int, α]})#λ],
        implicitly)
      (result.m.value, result.n.value)
    }

    val x = wordCountLineCount(text)
    x.println
  }
}

trait Prod[M[_], N[_], A] {
  val m: M[A]
  val n: N[A]

  def toTuple = (m, n)
}

object Prod {
  def prod[M[_], N[_], A](ma: M[A], na: N[A]) = new Prod[M, N, A] {
    val m = ma
    val n = na
  }

  def ⊗[M[_], N[_], A, B](fm: A => M[B], fn: A => N[B])(a: A): (Prod[M, N, B]) = prod(fm(a), fn(a))

  def ProdApplicative[M[_] : Applicative, N[_] : Applicative] = new Applicative[({type λ[α]=Prod[M, N, α]})#λ] {
    import Scalaz._

    def pure[A](a: => A): Prod[M, N, A] = prod(a.η[M], a.η[N])

    def apply[A, B](f: Prod[M, N, A => B], a: Prod[M, N, A]): Prod[M, N, B] = {
      lazy val fv = f;
      lazy val av = a;
      prod(av.m <*> fv.m, av.n <*> fv.n)
    }
  }
}
