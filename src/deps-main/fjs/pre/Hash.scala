package fjs.pre

import F._

object Hash {
  def hash[A](a: A)(implicit h: fj.pre.Hash[A]) = h hash a

  implicit def hash[A](f: A => Int): fj.pre.Hash[A] =
    fj.pre.Hash.hash[A](f andThen (x => x: java.lang.Integer))

  implicit val booleanHash = fj.pre.Hash.booleanHash
  implicit val byteHash = fj.pre.Hash.byteHash
  implicit val charHash = fj.pre.Hash.charHash
  implicit val doubleHash = fj.pre.Hash.doubleHash
  implicit val floatHash = fj.pre.Hash.floatHash
  implicit val intHash = fj.pre.Hash.intHash
  implicit val longHash = fj.pre.Hash.longHash
  implicit val shortHash = fj.pre.Hash.shortHash
  implicit val stringHash = fj.pre.Hash.stringHash
  implicit val stringBufferHash = fj.pre.Hash.stringBufferHash
  implicit val stringBuilderHash = fj.pre.Hash.stringBuilderHash

  implicit def eitherHash[A, B](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B]) = fj.pre.Hash.eitherHash(ha, hb)
  implicit def listHash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.listHash(ha)
  implicit def nonEmptyListHash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.nonEmptyListHash(ha)
  implicit def optionHash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.optionHash(ha)
  implicit def streamHash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.streamHash(ha)
  implicit def arrayHash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.arrayHash(ha)
  implicit def p1Hash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.p1Hash(ha)
  implicit def p2Hash[A, B](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B]) = fj.pre.Hash.p2Hash(ha, hb)
  implicit def p3Hash[A, B, C](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B], hc: fj.pre.Hash[C]) = fj.pre.Hash.p3Hash(ha, hb, hc)
  implicit def p4Hash[A, B, C, D](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B], hc: fj.pre.Hash[C], hd: fj.pre.Hash[D]) = fj.pre.Hash.p4Hash(ha, hb, hc, hd)
  implicit def p5Hash[A, B, C, D, E](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B], hc: fj.pre.Hash[C], hd: fj.pre.Hash[D], he: fj.pre.Hash[E]) = fj.pre.Hash.p5Hash(ha, hb, hc, hd, he)
  implicit def p6Hash[A, B, C, D, E, F$](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B], hc: fj.pre.Hash[C], hd: fj.pre.Hash[D], he: fj.pre.Hash[E], hf: fj.pre.Hash[F$]) = fj.pre.Hash.p6Hash(ha, hb, hc, hd, he, hf)
  implicit def p7Hash[A, B, C, D, E, F$, G](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B], hc: fj.pre.Hash[C], hd: fj.pre.Hash[D], he: fj.pre.Hash[E], hf: fj.pre.Hash[F$], hg: fj.pre.Hash[G]) = fj.pre.Hash.p7Hash(ha, hb, hc, hd, he, hf, hg)
  implicit def p8Hash[A, B, C, D, E, F$, G, H](implicit ha: fj.pre.Hash[A], hb: fj.pre.Hash[B], hc: fj.pre.Hash[C], hd: fj.pre.Hash[D], he: fj.pre.Hash[E], hf: fj.pre.Hash[F$], hg: fj.pre.Hash[G], hh: fj.pre.Hash[H]) = fj.pre.Hash.p8Hash(ha, hb, hc, hd, he, hf, hg, hh)
  implicit def treeHash[A](implicit ha: fj.pre.Hash[A]) = fj.pre.Hash.treeHash(ha)
}
