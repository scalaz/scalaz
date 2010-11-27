package scalaz

import Scalaz._
import scalaz.Category._
import scala.collection.generic.{Subtractable, Addable}
import scala.collection.{SetLike, SeqLike}
import scala.collection.immutable._
import scala._

/**
 * Lenses are required to satisfy the following two laws and to be side-effect free
 *
 * forall b. lens(lens.set(a,b)) = b
 * forall a. lens.set(a,lens(a)) = a
 */
case class Lens[A,B](get: A => B, set: (A,B) => A) extends Immutable {
  def apply(whole:A): B   = get(whole)
  def mod(a:A, f: B => B) = set(a, f(this(a)))
  def modf[F[_]:Functor](a:A, f: B => F[B]): F[A] = f(this(a)).map((b:B) => set(a,b))
  def modp[C](a: A, f: B => (B, C)): (A, C) = {
    val (b,c) = f(this(a))
    (set(a,b),c)
  }
  def compose[C](that: Lens[C,A]) = Lens[C,B](
    c => this(that(c)),
    (c, b) => that.mod(c, set(_, b))
  )
  def andThen[C](that: Lens[B,C]) = that compose this
  /** This requires that f and g witness an isomorphism between B and C */
  def xmap[C](f: B => C)(g: C => B) = Lens[A,C](
    a => f(this(a)),
    (a,c) => set(a,g(c))
  )
  def |||[C](that: Lens[C,B]) = Lens[Either[A,C],B](
    { case Left(a) => this(a)
      case Right(b) => that(b)
    },
    { case (Left(a),  b) => Left (set(a,b))
      case (Right(c), b) => Right(that.set(c,b))
    }
  )
  def ***[C,D](that: Lens[C,D]) = Lens[(A,C),(B,D)](
    ac => (this(ac._1), that(ac._2)),
    (ac,bd) => (set(ac._1,bd._1),that.set(ac._2,bd._2))
  )
  // these sadly fail lens laws
  // def &&&[C]  (that: Lens[A,C]): Lens[A,(B,C)]
  // def +++[C,D](that: Lens[C,D]): Lens[Either[A,C],Either[B,D]]

  def toState                 : State[A,B]    = state[A,B](a => (a, this(a)))

  // contravariantly map the state of a state monad through a lens
  def lifts[C](s: State[B,C]) : State[A,C]    = state[A,C](a => modp(a,s.apply))

  def modps[C](f: B => (B,C)) : State[A,C]    = lifts(state(f))

  def mods[C](f : B => B)     : State[A,B]    = state[A,B](a => modp(a,f(_).pair))
  def mods_[C](f : B => B)    : State[A,Unit] = state[A,Unit](a => (mod(a,f), Unit))
  def :=(b: B)                : State[A,Unit] = state[A,Unit](a => (set(a,b), Unit))

  def flatMap[C](f : B => State[A,C]) : State[A,C] = state[A,C](a => f(this(a))(a))
  def map[C](f: B => C) : State[A,C] = state[A,C](a => (a,f(this(a))))
}

object Lens { 
  implicit def category : Category[Lens] = new Category[Lens] {
    def compose[A,B,C](g: Lens[B,C], f: Lens[A,B]): Lens[A,C] = f andThen g
    def id[A]: Lens[A,A] = self[A] 
  }

  implicit def asState[A,B](lens: Lens[A,B]): State[A,B] = lens.toState

  implicit def invariantFunctor[A] : InvariantFunctor[PartialApply1Of2[Lens,A]#Apply] = 
    new InvariantFunctor[PartialApply1Of2[Lens,A]#Apply] {
      def xmap[B,C](lens: Lens[A,B], f: B => C, g: C => B) = lens.xmap[C](f)(g)
    }

  implicit def generalizedFunctor[A] : GeneralizedFunctor[Lens,Function1,Id] = 
    new GeneralizedFunctor[Lens,Function1,Id] {
      def fmap[A,B](lens: Lens[A,B]) = lens.apply
    }
 
  def fst[A,B]   = Lens[(A,B),A](_._1, (ab,a) => (a,ab._2))
  def snd[A,B]   = Lens[(A,B),B](_._2, (ab,b) => (ab._1,b))
  def self[A]    = Lens[A,A](a => a, (_, a) => a)
  def trivial[A] = Lens[A,Unit](_ => Unit, (a, _) => a)
  def codiag[A]  : Lens[Either[A,A],A] = self[A] ||| self[A]

  implicit def subtractableLens[S,A,Repr <: Subtractable[A,Repr]] = SubtractableLens[S,A,Repr](_)
  case class SubtractableLens[S,A,Repr <: Subtractable[A, Repr]](lens: Lens[A,Repr]) {
    def -=  (elem: A)                       = lens.mods (_ - elem)
    def -=  (elem1: A, elem2: A, elems: A*) = lens.mods (_ - elem1 - elem2 -- elems)
    def --= (xs: TraversableOnce[A])        = lens.mods (_ -- xs)
  }

  implicit def addableLens[S,A,Repr <: Addable[A,Repr]] = AddableLens[S,A,Repr](_)
  case class AddableLens[S,A,Repr <: Addable[A, Repr]](lens: Lens[A,Repr]) { 
    def += (elem: A) = lens.mods (_ + elem)
    def += (elem1: A, elem2: A, elems: A*) = lens.mods (_ + elem1 + elem2 ++ elems)
    def ++= (xs: TraversableOnce[A]) = lens.mods (_ ++ xs)
  }

  implicit def setLikeLens[S,K,Repr <: SetLike[K,Repr] with Set[K]] = SetLikeLens[S,K,Repr](_)
  case class SetLikeLens[S,K,Repr <: SetLike[K,Repr] with Set[K]](lens: Lens[S,Repr]) {
    def contains(key: K) = Lens[S,Boolean](
      s => lens(s).contains(key),
      (s, b) => lens.mod(s, m => if (b) m + key else m - key)
    )
    def &= (that: Set[K]) = lens.mods(_ & that)
    def &~=(that: Set[K]) = lens.mods(_ &~ that)
    def |= (that: Set[K]) = lens.mods(_ | that)
  }
  implicit def mapLens[S,K,V] = MapLens[S,K,V](_)

  case class MapLens[S,K,V](lens: Lens[S,Map[K,V]]) {
    def member(k:K) = Lens[S,Option[V]](
	s => lens(s).get(k), 
	(s, opt) => lens.mod(s, m => opt.cata(v => m + (k -> v), m - k)))
    // behavior undefined for accessing an element not present in the map!
    def at(k:K)     = Lens[S,V](lens.get(_)(k), (s, v) => lens.mod(s, _ + (k -> v)))
    def +=(elem1: (K,V), elem2: (K,V), elems: (K,V)*) = lens.mods (_ + elem1 + elem2 ++ elems)
    def +=(elem: (K,V)) = lens.mods (_ + elem) 
    def ++=(xs: TraversableOnce[(K,V)]) = lens.mods (_ ++ xs)
    def update(key: K, value: V) = lens.mods_(_.updated(key,value))
  }

  implicit def seqLens[S,A,Repr <: SeqLike[A,Repr]] = SeqLikeLens[S,A,Repr](_)
  case class SeqLikeLens[S, A, Repr <: SeqLike[A,Repr]](lens: Lens[S,Repr]) {
    def sortWith(lt: (A,A) => Boolean)          = lens.mods_(_ sortWith lt)
    def sortBy[B:math.Ordering](f:A=>B)              = lens.mods_(_ sortBy f)
    def sort[B >: A](implicit ord: math.Ordering[B]) = lens.mods_(_.sorted[B])
  }

  implicit def stackLens[S,A] = StackLens[S,A](_)
  case class StackLens[S,A](lens: Lens[S,Stack[A]]) { 
    def push(elem1: A, elem2: A, elems: A*) = lens.mods_(_ push elem1 push elem2 pushAll elems)
    def push(elem: A) = lens.mods_(_ push elem)
    def pop    = lens.mods_(_.pop)
    def pop2   = lens.modps(_.pop2.swap)
    def top    = lens.map(_.top)
    def length = lens.map(_.length)
  }

  implicit def queueLens[S,A] = QueueLens[S,A](_)
  case class QueueLens[S,A](lens: Lens[S,Queue[A]]) {
    def enqueue(elem: A) = lens.mods_(_ enqueue elem)
    def dequeue = lens.modps(_.dequeue.swap)
    def length = lens.map(_.length) 
  }

  implicit def arrayLens[S,A] = ArrayLens[S,Array[A]](_)
  case class ArrayLens[S,A](lens: Lens[S,Array[A]]) {
    def at(i : Int) = Lens[S,A](
      s => lens(s)(i),
      (s, v) => lens.mod(s, array => {
	val copy = array.clone() 
        copy.update(i,v)
        copy
      })
    )
    def length = lens.map(_.length)
  }

  implicit def numericLens[S,N:Numeric] = NumericLens[S,N](_)
  case class NumericLens[S,N](lens: Lens[S,N])(implicit num: Numeric[N]) {
    def +=(that:N) = lens.mods(num.plus(_,that))
    def -=(that:N) = lens.mods(num.minus(_,that))
    def *=(that:N) = lens.mods(num.times(_,that))
  }

  implicit def fractionalLens[S,F:Fractional] = FractionalLens[S,F](_)
  case class FractionalLens[S,F](lens: Lens[S,F])(implicit frac: Fractional[F]) { 
    def /=(that:F) = lens.mods(frac.div(_,that))
  }

  implicit def integralLens[S,I:Integral] = IntegralLens[S,I](_)
  case class IntegralLens[S,I](lens: Lens[S,I])(implicit int: Integral[I]) { 
    def %=(that:I) = lens.mods(int.quot(_,that))
  }
}
