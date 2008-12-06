package fjs.pre

import fj.pre.Ordering
import fj.data.{Option, Either, List, Stream, Array}
import F._

object Ord {
  def compare[A](a1: A, a2: A)(implicit oa: fj.pre.Ord[A]) = oa compare (a1, a2)

  implicit def ord[A](f: A => A => Ordering): fj.pre.Ord[A] =
    fj.pre.Ord.ord(f andThen (g => g: fj.F[A, Ordering]))

  implicit val ordBoolean = fj.pre.Ord.booleanOrd
  implicit val byteOrd = fj.pre.Ord.byteOrd
  implicit val charOrd = fj.pre.Ord.charOrd
  implicit val doubleOrd = fj.pre.Ord.doubleOrd
  implicit val floatOrd = fj.pre.Ord.floatOrd
  implicit val intOrd = fj.pre.Ord.intOrd
  implicit val longOrd = fj.pre.Ord.longOrd
  implicit val shortOrd = fj.pre.Ord.shortOrd
  implicit val stringOrd = fj.pre.Ord.stringOrd
  implicit val stringBufferOrd = fj.pre.Ord.stringBufferOrd
  implicit val stringBuilderOrd = fj.pre.Ord.stringBuilderOrd

  implicit def optionOrd[A](implicit oa: fj.pre.Ord[A]) = fj.pre.Ord.optionOrd(oa)
  implicit def eitherOrd[A, B](implicit oa: fj.pre.Ord[A], ob: fj.pre.Ord[B]) = fj.pre.Ord.eitherOrd(oa, ob)
  implicit def listOrd[A](implicit oa: fj.pre.Ord[A]) = fj.pre.Ord.listOrd(oa)
  implicit def nonEmptyListOrd[A](implicit oa: fj.pre.Ord[A]) = fj.pre.Ord.nonEmptyListOrd(oa)
  implicit def streamOrd[A](implicit oa: fj.pre.Ord[A]) = fj.pre.Ord.streamOrd(oa)
  implicit def arrayOrd[A](implicit oa: fj.pre.Ord[A]) = fj.pre.Ord.arrayOrd(oa)
}
