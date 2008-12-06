package fjs.pre

import F._

object Equal {
  def equal[A](a1: A, a2: A)(implicit ea: fj.pre.Equal[A]) = ea eq (a1, a2)
  
  implicit def equal[A](f: A => A => Boolean): fj.pre.Equal[A] =
    fj.pre.Equal.equal[A](f andThen (_ andThen (b => b: java.lang.Boolean)) andThen (g => g: fj.F[A, java.lang.Boolean]))

  implicit val booleanEqual = fj.pre.Equal.booleanEqual
  implicit val byteEqual = fj.pre.Equal.byteEqual
  implicit val charEqual = fj.pre.Equal.charEqual
  implicit val doubleEqual = fj.pre.Equal.doubleEqual
  implicit val floatEqual = fj.pre.Equal.floatEqual
  implicit val intEqual = fj.pre.Equal.intEqual
  implicit val longEqual = fj.pre.Equal.longEqual
  implicit val shortEqual = fj.pre.Equal.shortEqual
  implicit val stringEqual = fj.pre.Equal.stringEqual
  implicit val stringBufferEqual = fj.pre.Equal.stringBufferEqual
  implicit val stringBuilderEqual = fj.pre.Equal.stringBuilderEqual

  implicit def eitherEqual[A, B](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B]) = fj.pre.Equal.eitherEqual(ea, eb)
  implicit def listEqual[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.listEqual(ea)
  implicit def nonEmptyListEqual[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.nonEmptyListEqual(ea)
  implicit def optionEqual[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.optionEqual(ea)
  implicit def streamEqual[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.streamEqual(ea)
  implicit def arrayEqual[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.arrayEqual(ea)
  implicit def p1Equal[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.p1Equal(ea)
  implicit def p2Equal[A, B](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B]) = fj.pre.Equal.p2Equal(ea, eb)
  implicit def p3Equal[A, B, C](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B], ec: fj.pre.Equal[C]) = fj.pre.Equal.p3Equal(ea, eb, ec)
  implicit def p4Equal[A, B, C, D](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B], ec: fj.pre.Equal[C], ed: fj.pre.Equal[D]) = fj.pre.Equal.p4Equal(ea, eb, ec, ed)
  implicit def p5Equal[A, B, C, D, E](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B], ec: fj.pre.Equal[C], ed: fj.pre.Equal[D], ee: fj.pre.Equal[E]) = fj.pre.Equal.p5Equal(ea, eb, ec, ed, ee)
  implicit def p6Equal[A, B, C, D, E, F$](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B], ec: fj.pre.Equal[C], ed: fj.pre.Equal[D], ee: fj.pre.Equal[E], ef: fj.pre.Equal[F$]) = fj.pre.Equal.p6Equal(ea, eb, ec, ed, ee, ef)
  implicit def p7Equal[A, B, C, D, E, F$, G](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B], ec: fj.pre.Equal[C], ed: fj.pre.Equal[D], ee: fj.pre.Equal[E], ef: fj.pre.Equal[F$], eg: fj.pre.Equal[G]) = fj.pre.Equal.p7Equal(ea, eb, ec, ed, ee, ef, eg)
  implicit def p8Equal[A, B, C, D, E, F$, G, H](implicit ea: fj.pre.Equal[A], eb: fj.pre.Equal[B], ec: fj.pre.Equal[C], ed: fj.pre.Equal[D], ee: fj.pre.Equal[E], ef: fj.pre.Equal[F$], eg: fj.pre.Equal[G], eh: fj.pre.Equal[H]) = fj.pre.Equal.p8Equal(ea, eb, ec, ed, ee, ef, eg, eh)
  implicit def treeEqual[A](implicit ea: fj.pre.Equal[A]) = fj.pre.Equal.treeEqual(ea)
}
