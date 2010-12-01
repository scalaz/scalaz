package scalaz

import Scalaz._
import scalaz.Category._
import scala.collection.generic.{Subtractable, Addable}
import scala.collection.{SetLike, SeqLike}
import scala.collection.immutable._
import scala._

/**
 * Lenses are required to satisfy the following two laws and to be side-effect free.
 *
 * <p>
 * All instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. lens.set(a,lens(a)) = a</core></li>
 * <li><strong>retention</strong><br/><code>forall a b. lens(lens.set(a,b)) = b</core></li>
 * </ol>
 * </p>
 */

case class Lens[A,B](get: A => B, set: (A,B) => A) extends Immutable {
  /** A Lens[A,B] can be used as a function from A => B, or implicitly via Lens.asState as a State[A,B] action */
  def apply(whole:A): B = get(whole)

  /** Modify the value viewed through the lens */
  def mod(a:A, f: B => B) : A = set(a, f(get(a)))

  /** Modify the value viewed through the lens, a functor full of results */
  def modf[F[_]:Functor](a:A, f: B => F[B]): F[A] = f(get(a)).map((b:B) => set(a,b))

  /** modp[C] = modf[PartialApply1Of2[Tuple,C]#Flip], but is more convenient to think about */
  def modp[C](a: A, f: B => (B, C)): (A, C) = {
    val (b,c) = f(get(a))
    (set(a,b),c)
  }

  /** Lenses can be composed */
  def compose[C](that: Lens[C,A]) = Lens[C,B](
    c => get(that.get(c)),
    (c, b) => that.mod(c, set(_, b))
  )
  def andThen[C](that: Lens[B,C]) = that compose this

  /** You can apply an isomorphism to the value viewed through the lens to obtain a new lens. */
  def xmap[C](f: B => C)(g: C => B) = Lens[A,C](
    a => f(get(a)),
    (a,c) => set(a,g(c))
  )

  /** Two lenses that view a value of the same type can be joined */
  def |||[C](that: Lens[C,B]) = Lens[Either[A,C],B](
    { case Left(a) => get(a)
      case Right(b) => that.get(b)
    },
    { case (Left(a),  b) => Left (set(a,b))
      case (Right(c), b) => Right(that.set(c,b))
    }
  )

  /** Two disjoint lenses can be paired */
  def ***[C,D](that: Lens[C,D]) = Lens[(A,C),(B,D)](
    ac => (get(ac._1), that.get(ac._2)),
    (ac,bd) => (set(ac._1,bd._1),that.set(ac._2,bd._2))
  )

  // These two sadly fail lens laws:

  // &&& fails retention in the event that the two lenses overlap. Consider: fst &&& fst
  // def &&&[C]  (that: Lens[A,C]): Lens[A,(B,C)]

  // +++ fails retention because of set(Left(a), Right(d))
  // def +++[C,D](that: Lens[C,D]): Lens[Either[A,C],Either[B,D]]

  /** A Lens[A,B] can be used directly as a State[A,B] that retrieves the value viewed from the state */
  implicit def toState : State[A,B] = state[A,B](a => (a, get(a)))

  /** We can contravariantly map the state of a state monad through a lens */
  def lifts[C](s: State[B,C]) : State[A,C]    = state[A,C](a => modp(a,s.apply))

  /** modify the state, and return a derived value as a state monadic action. */
  def modps[C](f: B => (B,C)) : State[A,C]    = lifts(state(f))

  /** modify the portion of the state viewed through the lens and return its new value */
  def mods[C](f : B => B)     : State[A,B]    = state[A,B](a => modp(a,f(_).pair))

  /** modify the portion of the state viewed through the lens, but do not return its new value */
  def mods_[C](f : B => B)    : State[A,Unit] = state[A,Unit](a => (mod(a,f), Unit))

  /** Set the value viewed through the lens to a given value */
  def :=(b: B)                : State[A,Unit] = state[A,Unit](a => (set(a,b), Unit))

  /** flatMapping a lens yields a state action to avoid ambiguity */
  def flatMap[C](f : B => State[A,C]) : State[A,C] = state[A,C](a => f(get(a))(a))

  /** Mapping a lens yields a state action to avoid ambiguity */
  def map[C](f: B => C) : State[A,C] = state[A,C](a => (a,f(get(a))))

}

object Lens { 
  /** The identity lens for a given object */
  def self[A]    = Lens[A,A](a => a, (_, a) => a)

  /** The trivial lens that can retrieve Unit from anything */
  def trivial[A] = Lens[A,Unit](_ => Unit, (a, _) => a)

  /** A lens that discards the choice of Right or Left from Either */
  def codiag[A]  : Lens[Either[A,A],A] = self[A] ||| self[A]

  /** Access the first field of a tuple */
  def fst[A,B] = Lens[(A,B),A](_._1, (ab,a) => (a,ab._2))

  /** Access the second field of a tuple */
  def snd[A,B] = Lens[(A,B),B](_._2, (ab,b) => (ab._1,b))

  /** Lenses form a category */
  implicit def category : Category[Lens] = new Category[Lens] {
    def compose[A,B,C](g: Lens[B,C], f: Lens[A,B]): Lens[A,C] = f andThen g
    def id[A]: Lens[A,A] = self[A] 
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def asState[A,B](lens: Lens[A,B]): State[A,B] = lens.toState

  /** Lenses are an invariant functor. xmap can be used to transform a view into an isomorphic form */
  implicit def invariantFunctor[A] : InvariantFunctor[({type λ[α]=Lens[A, α]})#λ] =
    new InvariantFunctor[({type λ[α]=Lens[A, α]})#λ] {
      def xmap[B,C](lens: Lens[A,B], f: B => C, g: C => B) = lens.xmap[C](f)(g)
    }

  /** There exists a generalized functor from Lenses to Function1, which just forgets how to set the value */
  implicit def generalizedFunctor[A] : GeneralizedFunctor[Lens,Function1,Id] = 
    new GeneralizedFunctor[Lens,Function1,Id] {
      def fmap[A,B](lens: Lens[A,B]) = lens.get
    }

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple2Lens[S,A,B](lens: Lens[S,(A,B)]) = (
    Lens[S,A](s => lens.get(s)._1, (s,a) => lens.mod(s, t => t copy (_1 = a))),
    Lens[S,B](s => lens.get(s)._2, (s,a) => lens.mod(s, t => t copy (_2 = a)))
  )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple3Lens[S,A,B,C](lens: Lens[S,(A,B,C)]) = (
    Lens[S,A](s => lens.get(s)._1, (s,a) => lens.mod(s, t => t copy (_1 = a))),
    Lens[S,B](s => lens.get(s)._2, (s,a) => lens.mod(s, t => t copy (_2 = a))),
    Lens[S,C](s => lens.get(s)._3, (s,a) => lens.mod(s, t => t copy (_3 = a)))
  )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple4Lens[S,A,B,C,D](lens: Lens[S,(A,B,C,D)]) = (
    Lens[S,A](s => lens.get(s)._1, (s,a) => lens.mod(s, t => t copy (_1 = a))),
    Lens[S,B](s => lens.get(s)._2, (s,a) => lens.mod(s, t => t copy (_2 = a))),
    Lens[S,C](s => lens.get(s)._3, (s,a) => lens.mod(s, t => t copy (_3 = a))),
    Lens[S,D](s => lens.get(s)._4, (s,a) => lens.mod(s, t => t copy (_4 = a)))
  )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple5Lens[S,A,B,C,D,E](lens: Lens[S,(A,B,C,D,E)]) = (
    Lens[S,A](s => lens.get(s)._1, (s,a) => lens.mod(s, t => t copy (_1 = a))),
    Lens[S,B](s => lens.get(s)._2, (s,a) => lens.mod(s, t => t copy (_2 = a))),
    Lens[S,C](s => lens.get(s)._3, (s,a) => lens.mod(s, t => t copy (_3 = a))),
    Lens[S,D](s => lens.get(s)._4, (s,a) => lens.mod(s, t => t copy (_4 = a))),
    Lens[S,E](s => lens.get(s)._5, (s,a) => lens.mod(s, t => t copy (_5 = a)))
  )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple6Lens[S,A,B,C,D,E,F](lens: Lens[S,(A,B,C,D,E,F)]) = (
    Lens[S,A](s => lens.get(s)._1, (s,a) => lens.mod(s, t => t copy (_1 = a))),
    Lens[S,B](s => lens.get(s)._2, (s,a) => lens.mod(s, t => t copy (_2 = a))),
    Lens[S,C](s => lens.get(s)._3, (s,a) => lens.mod(s, t => t copy (_3 = a))),
    Lens[S,D](s => lens.get(s)._4, (s,a) => lens.mod(s, t => t copy (_4 = a))),
    Lens[S,E](s => lens.get(s)._5, (s,a) => lens.mod(s, t => t copy (_5 = a))),
    Lens[S,F](s => lens.get(s)._6, (s,a) => lens.mod(s, t => t copy (_6 = a)))
  )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple7Lens[S,A,B,C,D,E,F,G](lens: Lens[S,(A,B,C,D,E,F,G)]) = (
    Lens[S,A](s => lens.get(s)._1, (s,a) => lens.mod(s, t => t copy (_1 = a))),
    Lens[S,B](s => lens.get(s)._2, (s,a) => lens.mod(s, t => t copy (_2 = a))),
    Lens[S,C](s => lens.get(s)._3, (s,a) => lens.mod(s, t => t copy (_3 = a))),
    Lens[S,D](s => lens.get(s)._4, (s,a) => lens.mod(s, t => t copy (_4 = a))),
    Lens[S,E](s => lens.get(s)._5, (s,a) => lens.mod(s, t => t copy (_5 = a))),
    Lens[S,F](s => lens.get(s)._6, (s,a) => lens.mod(s, t => t copy (_6 = a))),
    Lens[S,G](s => lens.get(s)._7, (s,a) => lens.mod(s, t => t copy (_7 = a)))
  )

  trait WrappedLens[S,A] { 
    def lens : Lens[S,A]
  }

  /** A lens that views a Subtractable type can provide the appearance of in place mutation */
  implicit def subtractableLens[S,A,Repr <: Subtractable[A,Repr]](
    l : Lens[S,Repr]
  ) : SubtractableLens[S,A,Repr] = 
  new SubtractableLens[S,A,Repr] {
    def lens : Lens[S,Repr] = l
  }

  trait SubtractableLens[S,A,Repr <: Subtractable[A, Repr]] extends WrappedLens[S,Repr] {
    def -=  (elem: A)                       = lens.mods (_ - elem)
    def -=  (elem1: A, elem2: A, elems: A*) = lens.mods (_ - elem1 - elem2 -- elems)
    def --= (xs: TraversableOnce[A])        = lens.mods (_ -- xs)
  }

  /** A lens that views an Addable type can provide the appearance of in place mutation */
  implicit def addableLens[S,A,Repr <: Addable[A,Repr]](
    l: Lens[S,Repr]
  ) : AddableLens[S,A,Repr] = 
  new AddableLens[S,A,Repr] {
    def lens : Lens[S,Repr] = l
  }

  trait AddableLens[S,A,Repr <: Addable[A, Repr]] extends WrappedLens[S,Repr] { 
    def += (elem: A) = lens.mods (_ + elem)
    def += (elem1: A, elem2: A, elems: A*) = lens.mods (_ + elem1 + elem2 ++ elems)
    def ++= (xs: TraversableOnce[A]) = lens.mods (_ ++ xs)
  }

  /** A lens that views an SetLike type can provide the appearance of in place mutation */
  implicit def setLikeLens[S,K,Repr <: SetLike[K,Repr] with Set[K]](l: Lens[S,Repr]) : SetLikeLens[S,K,Repr] = new SetLikeLens[S,K,Repr] {
    def lens : Lens[S,Repr] = l
  }
  implicit def setLens[S,K] = setLikeLens[S,K,Set[K]](_)

  trait SetLikeLens[S,K,Repr <: SetLike[K,Repr] with Set[K]] extends AddableLens[S,K,Repr] with SubtractableLens[S,K,Repr] {
    /** Setting the value of this lens will change whether or not it is present in the set */
    def contains(key: K) = Lens[S,Boolean](
      s => lens.get(s).contains(key),
      (s, b) => lens.mod(s, m => if (b) m + key else m - key)
    )
    def &= (that: Set[K]) = lens.mods(_ & that)
    def &~=(that: Set[K]) = lens.mods(_ &~ that)
    def |= (that: Set[K]) = lens.mods(_ | that)
  }

  /** A lens that views an immutable Map type can provide a mutable.Map-like API via State */
  implicit def mapLens[S,K,V] = MapLens[S,K,V](_)
  case class MapLens[S,K,V](lens: Lens[S,Map[K,V]]) extends SubtractableLens[S,K,Map[K,V]] {
    /** Allows both viewing and setting the value of a member of the map */
    def member(k:K) = Lens[S,Option[V]](
        s => lens.get(s).get(k), 
        (s, opt) => lens.mod(s, m => opt.cata(v => m + (k -> v), m - k)))
    /** This lens has undefined behavior when accessing an element not present in the map! */
    def at(k:K)     = Lens[S,V](lens.get(_)(k), (s, v) => lens.mod(s, _ + (k -> v)))

    def +=(elem1: (K,V), elem2: (K,V), elems: (K,V)*) = lens.mods (_ + elem1 + elem2 ++ elems)
    def +=(elem: (K,V)) = lens.mods (_ + elem) 
    def ++=(xs: TraversableOnce[(K,V)]) = lens.mods (_ ++ xs)
    def update(key: K, value: V) = lens.mods_(_.updated(key,value))
  }

  /** Provide the appearance of a mutable-like API for sorting sequences through a lens */
  implicit def seqLikeLens[S,A,Repr <: SeqLike[A,Repr]] = SeqLikeLens[S,A,Repr](_)
  implicit def seqLens[S,A] = seqLikeLens[S,A,scala.collection.immutable.Seq[A]]
  case class SeqLikeLens[S, A, Repr <: SeqLike[A,Repr]](lens: Lens[S,Repr]) {
    def sortWith(lt: (A,A) => Boolean)          = lens.mods_(_ sortWith lt)
    def sortBy[B:math.Ordering](f:A=>B)              = lens.mods_(_ sortBy f)
    def sort[B >: A](implicit ord: math.Ordering[B]) = lens.mods_(_.sorted[B])
  }

  /** Provide an imperative-seeming API for stacks viewed through a lens */
  implicit def stackLens[S,A] = StackLens[S,A](_)
  case class StackLens[S,A](lens: Lens[S,Stack[A]]) { 
    def push(elem1: A, elem2: A, elems: A*) = lens.mods_(_ push elem1 push elem2 pushAll elems)
    def push(elem: A) = lens.mods_(_ push elem)
    def pop    = lens.mods_(_.pop)
    def pop2   = lens.modps(_.pop2.swap)
    def top    = lens.map(_.top)
    def length = lens.map(_.length)
  }

  /** Provide an imperative-seeming API for queues viewed through a lens */
  implicit def queueLens[S,A] = QueueLens[S,A](_)
  case class QueueLens[S,A](lens: Lens[S,Queue[A]]) {
    def enqueue(elem: A) = lens.mods_(_ enqueue elem)
    def dequeue = lens.modps(_.dequeue.swap)
    def length = lens.map(_.length) 
  }

  /** Provide an imperative-seeming API for arrays viewed through a lens */
  implicit def arrayLens[S,A] = ArrayLens[S,Array[A]](_)
  case class ArrayLens[S,A](lens: Lens[S,Array[A]]) {
    def at(i : Int) = Lens[S,A](
      s => lens.get(s)(i),
      (s, v) => lens.mod(s, array => {
        val copy = array.clone() 
        copy.update(i,v)
        copy
      })
    )
    def length = lens.map(_.length)
  }

  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  implicit def numericLens[S,N:Numeric](lens: Lens[S,N]) = NumericLens[S,N](lens, implicitly[Numeric[N]])
  case class NumericLens[S,N](lens: Lens[S,N], num : Numeric[N]) { 
    def +=(that:N) = lens.mods(num.plus(_,that))
    def -=(that:N) = lens.mods(num.minus(_,that))
    def *=(that:N) = lens.mods(num.times(_,that))
  }

  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  implicit def fractionalLens[S,F:Fractional](lens: Lens[S,F]) = FractionalLens[S,F](lens, implicitly[Fractional[F]])
  case class FractionalLens[S,F](lens: Lens[S,F], frac: Fractional[F]) { 
    def /=(that:F) = lens.mods(frac.div(_,that))
  }

  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  implicit def integralLens[S,I:Integral](lens: Lens[S,I]) = IntegralLens[S,I](lens, implicitly[Integral[I]])
  case class IntegralLens[S,I](lens: Lens[S,I], int: Integral[I]) { 
    def %=(that:I) = lens.mods(int.quot(_,that))
  }
}
