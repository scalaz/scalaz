package scalaz

import org.scalacheck.{Arbitrary, Gen}
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.list._
import scalaz.std.string._

object IndexedContsTTest extends SpecLite {

  type ContTListString[A] = ContT[List, String, A]
  type ContTListStringInt = ContTListString[Int]

  def contTListStringGen[A](implicit A: Arbitrary[A]): Gen[ContTListString[A]] = Gen.frequency(
    (1, Functor[Gen].map(Gen.containerOf[List, String](Gen.alphaStr))(l => ContT(f => l))),
    (1, Functor[Gen].map(A.arbitrary)(ContT.point(_))),
    (1, Functor[Gen].map(A.arbitrary)(i => ContT(f => {
      val l = f(i)
      val (a, b) = l.splitAt((l.size + 1) % 3)
      a ::: List("foo") ::: b
    })))
  )

  implicit def contTListStringIntArb: Arbitrary[ContTListStringInt] = Arbitrary(contTListStringGen[Int])
  implicit def contTListStringInt2Arb: Arbitrary[ContTListString[Int => Int]] = Arbitrary(contTListStringGen[Int => Int])
  implicit def contTListStringIntEqual: Equal[ContTListStringInt] = new Equal[ContTListStringInt] {
    def equal(a: ContTListStringInt, b: ContTListStringInt): Boolean = {
      val f: Int => List[String] = _.toString.toList.map(_.toString)
      Equal[List[String]].equal(a(f), b(f))
    }
  }

  checkAll(monadPlus.laws[ContTListString])

  object instances {
    def functorRight[W[_]: Functor, M[_], R, O] = Functor[IndexedContsT[W, M, R, O, ?]]
    def functorLeft[W[_], M[_]: Functor, O, A] = Functor[IndexedContsT[W, M, ?, O, A]]
    def contravariant[W[_]: Functor, M[_]: Functor, R, A] = Contravariant[IndexedContsT[W, M, R, ?, A]]
    def bifunctor[W[_]: Functor, M[_]: Functor, O] = Bifunctor[IndexedContsT[W, M, ?, O, ?]]
    def bind[W[_]: Cobind, M[_], R] = Bind[ContsT[W, M, R, ?]]
    def monad[W[_]: Comonad, M[_], R] = Monad[ContsT[W, M, R, ?]]

    // checking absence of ambiguity
    def functor[W[_]: Comonad, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def functor[W[_]: Cobind, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def bind[W[_]: Comonad, M[_], R] = Bind[ContsT[W, M, R, ?]]
  }
}
