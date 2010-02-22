package scalaz

import Scalaz._

object WordCount {
  def main(args: Array[String]) = wordCount

  /**
   * Character/Line/Word Count from "The Essense of the Iterator Pattern".
   *
   * This is an experiment to discover which parts of this paper can be brought to Scalaz.
   */
  def wordCount {
    def liftC[A, B](f: A => B) = {a: A => Const(f(a))}
    val charCountBody: (Char) => Const[Int, Nothing] = liftC(Function.const(1))
    def charCount(text: List[Char]): Const[Int, Any] = text.traverse[PartialApply1Of2[Const, Int]#Apply, Any](charCountBody)
    def test(p: Boolean): Int = if (p) 1 else 0
    val lineCountBody: (Char) => Const[Int, Nothing] = liftC {c: Char => test(c == '\n')}
    def lineCount(text: List[Char]): Const[Int, Any] = text.traverse[PartialApply1Of2[Const, Int]#Apply, Any](lineCountBody)

    val text = "the cat in the hat\n sat on the mat".toList

    (charCount(text): Int, lineCount(text): Int).println

    import Prod._

    val wordCountLineCountBody = ⊗[PartialApply1Of2[Const, Int]#Apply, PartialApply1Of2[Const, Int]#Apply, Char, Any](charCountBody, lineCountBody) _
    def wordCountLineCount(text: List[Char]) = {
      val result = text.traverse[PartialApplyProd[PartialApply1Of2[Const, Int]#Apply, PartialApply1Of2[Const, Int]#Apply]#Apply, Any](wordCountLineCountBody)(
        ProdApplicative[PartialApply1Of2[Const, Int]#Apply, PartialApply1Of2[Const, Int]#Apply],
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

  trait PartialApplyProd[M[_], N[_]] {
    type Apply[A] = Prod[M, N, A]
  }

  def ProdApplicative[M[_] : Applicative, N[_] : Applicative] = new Applicative[PartialApplyProd[M, N]#Apply] {
    import Scalaz._

    def pure[A](a: => A): Prod[M, N, A] = prod(a.η[M], a.η[N])

    def apply[A, B](f: Prod[M, N, A => B], a: Prod[M, N, A]): Prod[M, N, B] = prod(a.m <*> f.m, a.n <*> f.n)
  }
}

trait Comp[M[_], N[_], A] {
  def value: M[N[A]]
}

object Comp {
  import Scalaz._

  def comp[M[_], N[_], A](mna: M[N[A]]) = new Comp[M, N, A] {
    def value = mna
  }

  trait PartialApplyComp[M[_], N[_]] {
    type Apply[A] = Comp[M, N, A]
  }

  def ⊙[M[_] : Functor, N[_] : Functor, A, B, C](f: B => N[C], g: A => M[B])(a: A): Comp[M, N, C] =
    comp(g(a) ∘ f)

  def CompApplicative[M[_] : Applicative, N[_] : Applicative] = new Applicative[PartialApplyComp[M, N]#Apply] {
    def pure[A](a: => A): Comp[M, N, A] = comp(a.η[N].η[M])

    def apply[A, B](f: Comp[M, N, A => B], a: Comp[M, N, A]): Comp[M, N, B] = {
      comp(a.value <**> (f.value, _ <*> (_: N[A => B])))
    }
  }
}

trait Coerce[A, B] {
  def wrap(b: B): A

  def unwrap(a: A): B
}

object Coerce {
  import Scalaz._
  def IdentityCoerce[B]: Coerce[Identity[B], B] = new Coerce[Identity[B], B] {
    def wrap(b: B) = b

    def unwrap(a: Identity[B]) = a
  }
}
