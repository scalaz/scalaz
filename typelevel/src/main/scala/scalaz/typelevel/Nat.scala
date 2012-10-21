package scalaz
package typelevel

sealed trait Nat {

  type Self <: Nat

  def self: Self


  type Unapplied[U, Z <: U, P[_ <: Nat] <: U] <: U

  def unapplied[U, Z <: U, P[_ <: Nat] <: U](ifZero: => Z, ifSucc: => HStream[P]): Unapplied[U, Z, P]


  type Folded[U, F <: NFold[U]] <: U

  def fold[U, F <: NFold[U]](f: F): Folded[U, F]


  final def foldU[U](f: NFold[U]): Folded[U, f.type] = fold[U, f.type](f)

  final def toInt = foldU(NFold.ToInt)

  final def succ: Succ[Self] = Succ(self)

  final def pred: Unapplied[Option[Nat], None.type, ({type λ[α <: Nat] = Some[α]})#λ] = unapplied[Option[Nat], None.type, ({type λ[α <: Nat] = Some[α]})#λ](None, new HStream[({type λ[α <: Nat] = Some[α]})#λ] {
    def apply[N <: Nat](n: N) = Some(n)
  })

  // could as well be implemented with the `Unapplied` machinery, but it's easier this way

  final def isZero: Boolean = this match {
    case Zero => true
    case Succ(_) => false
  }

}

case object Zero extends Nat {

  type Self = Zero.type

  def self = this


  override type Unapplied[U, Z <: U, P[_ <: Nat] <: U] = Z

  def unapplied[U, Z <: U, P[_ <: Nat] <: U](ifZero: => Z, ifSucc: => HStream[P]): Unapplied[U, Z, P] = ifZero


  override type Folded[U, F <: NFold[U]] = F#Zero

  def fold[U, F <: NFold[U]](f: F): Folded[U, F] = f.zero

}

case class Succ[N <: Nat](predecessor: N) extends Nat {

  type Self = Succ[N]

  def self = this


  override type Unapplied[U, Z <: U, P[_ <: Nat] <: U] = P[N]

  def unapplied[U, Z <: U, P[_ <: Nat] <: U](ifZero: => Z, ifSucc: => HStream[P]): Unapplied[U, Z, P] = ifSucc(predecessor)


  override type Folded[U, F <: NFold[U]] = F#Succ[predecessor.Folded[U, F]]

  def fold[U, F <: NFold[U]](f: F): Folded[U, F] = f.succ(predecessor.fold[U, F](f))

}

trait Nats {

  type _0 = Zero.type
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]

  val _0 = Zero
  lazy val _1 = Succ(_0)
  lazy val _2 = Succ(_1)
  lazy val _3 = Succ(_2)
  lazy val _4 = Succ(_3)
  lazy val _5 = Succ(_4)
  lazy val _6 = Succ(_5)
  lazy val _7 = Succ(_6)
  lazy val _8 = Succ(_7)
  lazy val _9 = Succ(_8)

}

object Nats extends Nats

// vim: expandtab:ts=2:sw=2
