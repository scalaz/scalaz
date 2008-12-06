package fjs.data

import fj.data.Stream.{nil, cons}
import fj.P1
import fj.Function.curry
import F2._

object Stream {
  implicit def ScalaStream_Stream[A](as: scala.Stream[A]): fj.data.Stream[A] =
    if(as.isEmpty) nil[A] else cons(as.head, new P1[fj.data.Stream[A]] {
      def _1 = as.tail
    })

  implicit def Stream_ScalaStream[A](as: fj.data.Stream[A]): scala.Stream[A] =
    as.foldRight(curry((a: A, p: P1[Stream[A]]) => scala.Stream.cons(a, p._1)), scala.Stream.empty)

  def stream[A](h: A, t: => fj.data.Stream[A]) = fj.data.Stream.cons(h, new P1[fj.data.Stream[A]] {
    lazy val tt = t
    override def _1 = tt
  })

  object :| {
    def unapply[A](as: fj.data.Stream[A]) =
      if(as.isEmpty) None
      else Some(as.head, as.tail._1)
  }
}
