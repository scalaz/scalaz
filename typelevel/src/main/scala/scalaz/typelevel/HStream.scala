package scalaz
package typelevel

object HStream {

  def fromForall[T[_]](elems: Forall[T]): HStream[T] = new HStream[T] {
    def apply[N <: Nat](n: N) = elems[N]
  }

  def const[A](elem: A): HStream[Konst[A]#Apply] = new HStream[Konst[A]#Apply] {
    def apply[N <: Nat](n: N) = elem
  }

}

trait AbstractHStream {

  type Prepended[A] <: AbstractHStream

  def prepend[A](a: A): Prepended[A]

  def ::[A](a: A) = prepend(a)


  type Tail <: AbstractHStream

  def tail: Tail

}

trait HStream[T[_ <: Nat]] extends AbstractHStream { self =>

  import HFold._
  import HStream._
  import Nats._


  // index

  type Index[I <: Nat] = T[I]

  def apply[N <: Nat](n: N): Index[N]

  def at[N <: Nat](n: N) = apply(n)


  // head

  type Head = T[_0]

  def head: T[_0] = apply(_0)


  // prepend

  override type Prepended[A] = HStream[({ type λ[N <: Nat] = N#Unapplied[A, T] })#λ]

  def prepend[A](a: A): Prepended[A] = new Prepended[A] {
    def apply[N <: Nat](n: N) = n.unapplied(a, self)
  }

  def +:[M[_]](list: GenericList[M]): list.Folded[M, AbstractHStream, PrependToHStream[M, T]] =
    list.fold[M, AbstractHStream, PrependToHStream[M, T]](new PrependToHStream[M, T](this))

  def prependList[M[_]](list: GenericList[M]) = list +: this


  // tail

  override type Tail = HStream[({ type λ[α <: Nat] = T[Succ[α]] })#λ]

  def tail: Tail = new Tail {
    def apply[N <: Nat](n: N) = self(Succ(n))
  }


  // drop

  object Drop extends NFold[AbstractHStream] {
    type Zero = self.type
    def zero: Zero = self

    type Succ[N <: AbstractHStream] = N#Tail
    def succ[N <: AbstractHStream](n: N): Succ[N] = n.tail
  }

  type Dropped[L <: Nat] = L#Folded[AbstractHStream, Drop.type]

  def drop[L <: Nat](length: L): Dropped[L] = length.fold[AbstractHStream, Drop.type](Drop)


  // point

  type Point[P[_]] = HStream[({ type λ[α <: Nat] = P[T[α]] })#λ]

  def point[P[_] : Pointed]: Point[P] = new Point[P] {
    def apply[N <: Nat](n: N) = Pointed[P].point(self(n))
  }

}

// vim: expandtab:ts=2:sw=2
