package scalaz
package typelevel

trait Numerals {

  sealed trait Nat {

    type Folded[U, F <: NFold[U]] <: U

    def fold[U, F <: NFold[U]](f: F): Folded[U, F]

    final def value = fold[Int, NFold.ToInt](NFold.toInt)

  }

  case object Zero extends Nat {

    type Folded[U, F <: NFold[U]] = F#Zero

    def fold[U, F <: NFold[U]](f: F): Folded[U, F] = f.zero

  }

  case class Succ[N <: Nat](predecessor: N) extends Nat {

    type Folded[U, F <: NFold[U]] = F#Succ[predecessor.Folded[U, F]]

    def fold[U, F <: NFold[U]](f: F): Folded[U, F] = f.succ(predecessor.fold[U, F](f))

  }

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

/*
  // Doesn't work as expected: compiling `implicitly[_3]` makes scalac loop

  implicit def zero = Zero
  
  implicit def succ[N <: Nat](implicit predecessor: N): Succ[N] = Succ(predecessor)
*/

}

// vim: expandtab:ts=2:sw=2

