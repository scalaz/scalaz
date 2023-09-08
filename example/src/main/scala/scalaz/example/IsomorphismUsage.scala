package scalaz.example

import scalaz._, Isomorphism._
import std.list._
import std.anyVal._
import syntax.monoid._
import syntax.monad._
import syntax.equal._
import syntax.traverse._

object IsomorphismUsage {
  def isoSet[A] = new IsoSet[Seq[A], List[A]] {
    def to: Seq[A] => List[A] = _.toList
    def from: List[A] => Seq[A] = _.toSeq
  } // or simply:
  def equivalentIsoSet[A] = IsoSet[Seq[A], List[A]](_.toList, _.toSeq)


  def isoFunctor: Seq <~> List = new IsoFunctorTemplate[Seq, List] {
    def to_[A](sa: Seq[A]): List[A] = sa.toList
    def from_[A](la: List[A]): Seq[A] = la.toSeq
  }

  def main(args: Array[String]): Unit = {
    implicit def isoMonoid[A]: IsomorphismMonoid[Seq[A], List[A]] = new IsomorphismMonoid[Seq[A], List[A]] {
      def G = Monoid[List[A]]
      def iso: Seq[A] <=> List[A] = isoSet
    }
    implicit val isoMonad: IsomorphismMonad[Seq, List] = new IsomorphismMonad[Seq, List] {
      def G = Monad[List]
      def iso: Seq <~> List = isoFunctor
    }
    implicit val isoTraverse: IsomorphismTraverse[Seq, List] = new IsomorphismTraverse[Seq, List] {
      def G = Traverse[List]
      def iso: Seq <~> List  = isoFunctor
    }

    assert((Seq(1, 2) |+| Seq(3, 4)).toList === List(1, 2, 3, 4))
    assert {
      val seq = Seq(1, 2, 3) >>= {x => Seq(x, x+1)}
      val lst = List(1, 2, 3) >>= {x => List(x, x+1)}
      seq.toList === lst
    }
    assert(List(Seq(1, 2), Seq(3, 4)).sequence.toList === List(List(1, 2), List(3, 4)).sequence)
  }
}
