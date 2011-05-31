package scalaz
package data

import CoStateT._
import collection.immutable.Stack

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
sealed trait Lens[A, B] {
  val run: A => CoState[B, A]

  import StateT._
  import Lens._

  def *->* : (({type λ[α] = A @@ α})#λ *->* B) =
    data.*->*.**->**[({type λ[α] = A @@ α})#λ, B](this)

  def get: A => B =
    a =>
      run(a).pos

  def apply(a: A): B =
    get(a)

  def set: A => B => A =
    a =>
      run(a).put

  /**Modify the value viewed through the lens */
  def mod(f: B => B): A => A =
    a =>
      set(a)(f(get(a)))

  /**Modify the value viewed through the lens, a functor full of results */
  def modf[F[_]](f: B => F[B])(implicit ftr: Functor[F]): A => F[A] =
    a =>
      ftr.fmap((b: B) => set(a)(b))(f(get(a)))

  def access: State[A, B] =
    %=(z => z)

  def :=(b: => B): State[A, B] =
    %=(_ => b)

  def %=(f: B => B): State[A, B] =
    state[A, B](a => {
      val b = f(get(a))
      (b, set(a)(b))
    })

  def %==(f: B => B): State[A, Unit] =
    state[A, Unit](a => {
      ((), mod(f)(a))
    })

  def %%=[C](s: State[B, C]): State[A, C] =
    state[A, C](a => {
      val r = s.run
      val (c, b) = r(get(a))
      (c, set(a)(b))
    })

  def >-[C](f: B => C): State[A, C] =
    state[A, C](a => (f(get(a)), a))

  def >>-[C](f: B => State[A, C]): State[A, C] =
    state[A, C](a => {
      val r = f(get(a)).run
      r(a)
    })

  /**Lenses can be composed */
  def >=>[C](that: C @@ A): (C @@ B) =
    lens[C, B](c => {
      val (f, a) = that.run(c).run
      val (g, b) = run(a).run
      coState[B, C](f compose g, b)
    })

  /**Lenses can be composed */
  def <=<[C](that: B @@ C): (A @@ C) =
    that >=> this

  /**Two lenses that view a value of the same type can be joined */
  def |||[C](that: C @@ B): (Either[A, C] @@ B) =
    lensGG[Either[A, C], B](
    {
      case Left(a) => get(a)
      case Right(b) => that.get(b)
    }, {
      case (Left(a), b) => Left(set(a)(b))
      case (Right(c), b) => Right(that.set(c)(b))
    }
    )

  /**Two disjoint lenses can be paired */
  def ***[C, D](that: C @@ D): ((A, C) @@ (B, D)) =
    lensGG[(A, C), (B, D)](
      ac => (get(ac._1), that.get(ac._2)),
      (ac, bd) => (set(ac._1)(bd._1), that.set(ac._2)(bd._2))
    )

  def tuple2[C, D](implicit i: B =:= (C, D), j: (C, D) =:= B): (A @@ C, A @@ D) =
    (
        lensG(a => get(a)._1, a => b => mod(t => t copy (_1 = b))(a))
        , lensG(a => get(a)._2, a => b => mod(t => t copy (_2 = b))(a))
        )

  def tuple3[C, D, E](implicit i: B =:= (C, D, E), j: (C, D, E) =:= B): (A @@ C, A @@ D, A @@ E) =
    (
        lensG(a => get(a)._1, a => b => mod(t => t copy (_1 = b))(a))
        , lensG(a => get(a)._2, a => b => mod(t => t copy (_2 = b))(a))
        , lensG(a => get(a)._3, a => b => mod(t => t copy (_3 = b))(a))
        )

  def tuple4[C, D, E, F](implicit i: B =:= (C, D, E, F), j: (C, D, E, F) =:= B): (A @@ C, A @@ D, A @@ E, A @@ F) =
    (
        lensG(a => get(a)._1, a => b => mod(t => t copy (_1 = b))(a))
        , lensG(a => get(a)._2, a => b => mod(t => t copy (_2 = b))(a))
        , lensG(a => get(a)._3, a => b => mod(t => t copy (_3 = b))(a))
        , lensG(a => get(a)._4, a => b => mod(t => t copy (_4 = b))(a))
        )

  def tuple5[C, D, E, F, G](implicit i: B =:= (C, D, E, F, G), j: (C, D, E, F, G) =:= B): (A @@ C, A @@ D, A @@ E, A @@ F, A @@ G) =
    (
        lensG(a => get(a)._1, a => b => mod(t => t copy (_1 = b))(a))
        , lensG(a => get(a)._2, a => b => mod(t => t copy (_2 = b))(a))
        , lensG(a => get(a)._3, a => b => mod(t => t copy (_3 = b))(a))
        , lensG(a => get(a)._4, a => b => mod(t => t copy (_4 = b))(a))
        , lensG(a => get(a)._5, a => b => mod(t => t copy (_5 = b))(a))
        )

  def tuple6[C, D, E, F, G, H](implicit i: B =:= (C, D, E, F, G, H), j: (C, D, E, F, G, H) =:= B): (A @@ C, A @@ D, A @@ E, A @@ F, A @@ G, A @@ H) =
    (
        lensG(a => get(a)._1, a => b => mod(t => t copy (_1 = b))(a))
        , lensG(a => get(a)._2, a => b => mod(t => t copy (_2 = b))(a))
        , lensG(a => get(a)._3, a => b => mod(t => t copy (_3 = b))(a))
        , lensG(a => get(a)._4, a => b => mod(t => t copy (_4 = b))(a))
        , lensG(a => get(a)._5, a => b => mod(t => t copy (_5 = b))(a))
        , lensG(a => get(a)._6, a => b => mod(t => t copy (_6 = b))(a))
        )

  def tuple7[C, D, E, F, G, H, I](implicit i: B =:= (C, D, E, F, G, H, I), j: (C, D, E, F, G, H, I) =:= B): (A @@ C, A @@ D, A @@ E, A @@ F, A @@ G, A @@ H, A @@ I) =
    (
        lensG(a => get(a)._1, a => b => mod(t => t copy (_1 = b))(a))
        , lensG(a => get(a)._2, a => b => mod(t => t copy (_2 = b))(a))
        , lensG(a => get(a)._3, a => b => mod(t => t copy (_3 = b))(a))
        , lensG(a => get(a)._4, a => b => mod(t => t copy (_4 = b))(a))
        , lensG(a => get(a)._5, a => b => mod(t => t copy (_5 = b))(a))
        , lensG(a => get(a)._6, a => b => mod(t => t copy (_6 = b))(a))
        , lensG(a => get(a)._7, a => b => mod(t => t copy (_7 = b))(a))
        )

  /**Provide an imperative-seeming API for stacks viewed through a lens */
  case class StackLens[C](implicit i: B =:= Stack[C], j: Stack[C] =:= B) {
    def push(elem1: C, elem2: C, elems: C*): State[A, Unit] =
      %==(b => j(i(b) push elem1 push elem2 pushAll elems))

    def pop: State[A, Unit] =
      %==(b => j(i(b).pop))

    def pop2: State[A, C] =
      %%=(state(b => {
        val (c, s) = i(b).pop2
        (c, j(s))
      }))

    def top: State[A, C] =
      >-(i(_).top)

    def length: State[A, Int] =
      >-(i(_).length)
  }

  def stack[C](implicit i: B =:= Stack[C], j: Stack[C] =:= B) =
    StackLens[C]

  import collection.immutable.Queue

  /**Provide an imperative-seeming API for queues viewed through a lens */
  case class QueueLens[C](implicit i: B =:= Queue[C], j: Queue[C] =:= B) {
    def enqueue(elem: C): State[A, Unit] =
      %==(b => j(i(b).enqueue(elem)))

    def dequeue: State[A, C] =
      %%=(state(b => {
        val (c, _) = i(b).dequeue
        (c, j(b))
      }))

    def length: State[A, Int] =
      >-(i(_).length)
  }

  def queue[C](implicit i: B =:= Queue[C], j: Queue[C] =:= B) =
    QueueLens[C]

  /**Provide an imperative-seeming API for arrays viewed through a lens */
  case class ArrayLens[C](implicit i: B =:= Array[C], j: Array[C] =:= B) {
    def at(n: Int): (A @@ C) =
      lensG[A, C](
        a => i(get(a))(n),
        a => v => mod(array => {
          val copy = array.clone()
          copy.update(n, v)
          copy
        })(a)
      )

    def length: State[A, Int] =
      >-(i(_).length)
  }

  def array[C](implicit i: B =:= Array[C], j: Array[C] =:= B) =
    ArrayLens[C]

  /**Allow the illusion of imperative updates to numbers viewed through a lens */
  case class NumericLens(implicit num: Numeric[B]) {
    def +=(that: B): State[A, B] =
      %=(n => num.minus(n, that))

    def -=(that: B): State[A, B] =
      %=(n => num.minus(n, that))

    def *=(that: B): State[A, B] =
      %=(n => num.times(n, that))
  }

  def numeric(implicit num: Numeric[B]) =
    NumericLens

  /**Allow the illusion of imperative updates to numbers viewed through a lens */
  case class FractionalLens(implicit frac: Fractional[B]) {
    def /=(that: B): State[A, B] =
      %=(n => frac.div(n, that))
  }

  def fractional(implicit frac: Fractional[B]) =
    FractionalLens

  /**Allow the illusion of imperative updates to numbers viewed through a lens */
  case class IntegralLens(implicit ig: Integral[B]) {
    def %=(that: B): State[A, B] =
      Lens.this.%=(n => ig.quot(n, that))
  }

  def integral(implicit ig: Integral[B]) =
    IntegralLens
}

object Lens extends Lenss {
  def apply[A, B](r: A => CoState[B, A]): (A @@ B) =
    lens(r)
}

trait Lenss {
  type @@[A, B] =
  Lens[A, B]

  def lens[A, B](r: A => CoState[B, A]): (A @@ B) = new (A @@ B) {
    val run = r
  }

  def lensG[A, B](get: A => B, set: A => B => A): (A @@ B) =
    lens(a => coState((set(a), get(a))))

  def lensGG[A, B](get: A => B, set: (A, B) => A): (A @@ B) =
    lensG(get, a => b => set(a, b))

  /**The identity lens for a given object */
  def lensId[A]: (A @@ A) =
    lensG(z => z, _ => z => z)

  /**The trivial lens that can retrieve Unit from anything */
  def trivialLens[A]: (A @@ Unit) =
    lensG[A, Unit](_ => (), a => _ => a)

  /**A lens that discards the choice of Right or Left from Either */
  def codiagLens[A]: (Either[A, A] @@ A) =
    lensId[A] ||| lensId[A]

  /**Access the first field of a tuple */
  def firstLens[A, B]: ((A, B) @@ A) =
    lensG[(A, B), A](_._1, ab => a => (a, ab._2))

  /**Access the second field of a tuple */
  def secondLens[A, B]: ((A, B) @@ B) =
    lensG[(A, B), B](_._2, ab => b => (ab._1, b))

  implicit def LensId: Id[Lens] = new Id[Lens] {
    def id[A] = lensId
  }

  implicit def LensCompose: Compose[Lens] = new Compose[Lens] {
    def compose[A, B, C](f: (B @@ C), g: (A @@ B)) =
      f >=> g
  }

  implicit def LensCategory: Category[Lens] =
    Category.category
}