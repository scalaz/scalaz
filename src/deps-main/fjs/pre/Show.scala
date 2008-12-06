package fjs.pre

import fj.data.List
import F._

object Show {
  implicit def show[A](f: A => List[java.lang.Character]): fj.pre.Show[A] =
    fj.pre.Show.show(f)

  implicit val booleanShow = fj.pre.Show.booleanShow
  implicit val byteShow = fj.pre.Show.byteShow
  implicit val charShow = fj.pre.Show.charShow
  implicit val doubleShow = fj.pre.Show.doubleShow
  implicit val floatShow = fj.pre.Show.floatShow
  implicit val intShow = fj.pre.Show.intShow
  implicit val longShow = fj.pre.Show.longShow
  implicit val shortShow = fj.pre.Show.shortShow
  implicit val stringShow = fj.pre.Show.stringShow

  implicit def optionShow[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.optionShow(sa)
  implicit def eitherShow[A, B](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B]) = fj.pre.Show.eitherShow(sa, sb)
  implicit def listShow[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.listShow(sa)
  implicit def nonEmptyListShow[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.nonEmptyListShow(sa)
  implicit def treeShow[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.treeShow(sa)
  implicit def streamShow[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.streamShow(sa)
  implicit def arrayShow[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.arrayShow(sa)
  implicit def classShow[A] = fj.pre.Show.classShow[A]
  implicit def p1Show[A](implicit sa: fj.pre.Show[A]) = fj.pre.Show.p1Show(sa)
  implicit def p2Show[A, B](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B]) = fj.pre.Show.p2Show(sa, sb)
  implicit def p3Show[A, B, C](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B], sc: fj.pre.Show[C]) = fj.pre.Show.p3Show(sa, sb, sc)
  implicit def p4Show[A, B, C, D](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B], sc: fj.pre.Show[C], sd: fj.pre.Show[D]) = fj.pre.Show.p4Show(sa, sb, sc, sd)
  implicit def p5Show[A, B, C, D, E](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B], sc: fj.pre.Show[C], sd: fj.pre.Show[D]) = fj.pre.Show.p4Show(sa, sb, sc, sd)
  implicit def p6Show[A, B, C, D, E, F$](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B], sc: fj.pre.Show[C], sd: fj.pre.Show[D], se: fj.pre.Show[E], sf: fj.pre.Show[F$]) = fj.pre.Show.p6Show(sa, sb, sc, sd, se, sf)
  implicit def p7Show[A, B, C, D, E, F$, G](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B], sc: fj.pre.Show[C], sd: fj.pre.Show[D], se: fj.pre.Show[E], sf: fj.pre.Show[F$], sg: fj.pre.Show[G]) = fj.pre.Show.p7Show(sa, sb, sc, sd, se, sf, sg)
  implicit def p8Show[A, B, C, D, E, F$, G, H](implicit sa: fj.pre.Show[A], sb: fj.pre.Show[B], sc: fj.pre.Show[C], sd: fj.pre.Show[D], se: fj.pre.Show[E], sf: fj.pre.Show[F$], sg: fj.pre.Show[G], sh: fj.pre.Show[H]) = fj.pre.Show.p8Show(sa, sb, sc, sd, se, sf, sg, sh)
}
