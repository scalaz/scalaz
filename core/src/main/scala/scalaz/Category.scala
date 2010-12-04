package scalaz

import Scalaz.{⊥, ⊤}

/**
 * Defines a category.
 *
 * <p>
 * All instances must satisfy 3 laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>∀ a. compose(id, a) == a</code></li>
 * <li><strong>right identity</strong><br/><code>∀ a. compose(a, id) == a</code></li>
 * <li><strong>associativity</strong><br/><code>∀ a b c. compose(a, compose(b, c)) == compose(compose(a, b), c)</code></li>
 * </ol>
 * </p>
 */

trait Hom { 
  type L
  type H>:L
  type C[_ >: L <: H, _ >: L <: H]
}

trait GeneralizedCategory { 
  type U <: Hom
  type =>:[A >: U#L <: U#H, B >: U#L <: U#H] = U#C[A, B]

  def id[A >: U#L <: U#H]: A =>: A
  def compose[A >: U#L <: U#H, B >: U#L <: U#H, C >: U#L <: U#H](
    f: B =>: C,
    g: A =>: B
  ): A =>: C
  def *[UY<:Hom](that : GeneralizedCategory {type U=UY}) = Category.ProductCategory[U,UY](this,that)
}

trait GeneralizedGroupoid extends GeneralizedCategory { 
  def invert[A >: U#L <: U#H, B >: U#L <: U#H](f : A =>: B): B =>: A
}

trait Category[~>:[_,_]] extends GeneralizedCategory { 
  trait U extends Hom { 
    type L = ⊥
    type H = ⊤
    type C[A, B] = ~>:[A, B]
  }
}

trait Groupoid[~>:[_, _]] extends GeneralizedGroupoid with Category[~>:]

object Category {
  import Scalaz._
  import Leibniz._

  /* 
   * Product Categories
   */

  /** Index for a product category */
  sealed trait P[+IX, +IY] { type _1 = IX; type _2 = IY }

  case class ProductCategory[UX <: Hom, UY <: Hom](
    _1: GeneralizedCategory {type U = UX}, _2: GeneralizedCategory {type U = UY} 
  ) extends GeneralizedCategory with Hom {
    type _1 = _1.type
    type _2 = _2.type
    type L = P[UX#L, UY#L]
    type H = P[UX#H, UY#H]
    case class C[A >: L <: H, B >: L <: H](
      _1: UX#C[A#_1, B#_1], _2: UY#C[A#_2, B#_2]
    ) extends P[UX#C[A#_1, B#_1], UY#C[A#_2, B#_2]]
    type U = ProductCategory[UX, UY]

    def id[A>:U#L<:U#H] = C(_1.id[A#_1],_2.id[A#_2])
    def compose[A >: U#L <: U#H, B >: U#L <: U#H, C >: U#L <: U#H](
      f: B =>: C, g: A =>: B
    ) = C(_1.compose(f._1, g._1), _2.compose(f._2, g._2))
  }

  implicit def productCategory[UX <: Hom, UY <: Hom](
    implicit x: GeneralizedCategory {type U=UX},
             y: GeneralizedCategory {type U=UY}
  ) = ProductCategory[UX, UY](x, y)

  sealed class MonoidCategory[M](
    implicit monoid : Monoid[M]
  ) extends GeneralizedCategory with Hom {
    type L = ⊥
    type H = ⊥
    type C[A <: ⊥, B <: ⊥] = M
    type U = MonoidCategory[M]
    def id[A <: ⊥] = monoid.zero
    def compose[A <: ⊥, B <: ⊥, C <: ⊥](m: M, n: M): M = monoid.append(m, n)
  }
  implicit def monoidCategory[M:Monoid] : MonoidCategory[M] = new MonoidCategory[M]
  
  /** The <b>Set</b> category **/
  implicit val Function1Category: Category[Function1] = new Category[Function1] {
    def id[A] = a => a
    def compose[X, Y, Z](f: Y => Z, g: X => Y) = f compose g   
  }

  implicit val `<:<_Category` : Category[<:<] = new Category[<:<] {
    def compose[X, Y, Z](f: <:<[Y, Z], g: <:<[X, Y]) = f.asInstanceOf[X <:< Z]

    def id[A] = implicitly[A <:< A]
  }

  implicit val `=:=_Category` : Category[=:=] = new Category[=:=] {
    def compose[X, Y, Z](f: =:=[Y, Z], g: =:=[X, Y]) = f.asInstanceOf[X =:= Z]

    def id[A] = implicitly[A =:= A]
  }

  /** The flipped Function1 type */
  case class <=[A,B](value: B => A) extends NewType[B => A]

  /** The opposite category of the <b>Set</b> category. */
  implicit val OpCategory: Category[<=] = new Category[<=] {
    def id[A] = <=((x: A) => x)
    def compose[X, Y, Z](f: Y <= Z, g: X <= Y): X <= Z =
      <=(f.value andThen g.value)
  }

  /** Isomorphism for arrows of kind * -> * -> * */
  case class Iso[Arr[_,_], A, B](to: Arr[A, B], from: Arr[B, A])

  /** Isomorphism for arrows of kind (* -> *) -> (* -> *) -> * */
  case class Iso2[Arr[_[_], _[_]], F[_], G[_]](to: Arr[F,G], from: Arr[G,F])

  /** Isomorphism for arrows of kind (* -> * -> *) -> (* -> * -> *) -> * */
  case class Iso3[Arr[_[_,_], _[_,_]], F[_,_], G[_,_]](to: Arr[F,G], from: Arr[G,F])

  /** Set isomorphism */
  type <=>[A, B] = Iso[Function1, A, B]

  /** Natural isomorphism between functors */
  type <~>[F[_], G[_]] = Iso2[~>, F, G]

  /** Isomorphism natural in both sides of a bifunctor */
  type <~~>[F[_,_], G[_,_]] = Iso3[~~>, F, G]

  /** Set isomorphism is commutative */
  implicit def flipIso[A, B](implicit i: A <=> B): B <=> A =
    new Iso[Function1, B, A](i.from, i.to)

  /** Natural isomorphism is commutative */
  implicit def flipFunctorIso[F[_], G[_]](implicit i: F <~> G): G <~> F =
    new Iso2[~>, G, F](i.from, i.to)

  /** Every NewType is isomorphic to its underlying type.
      If its constructor is made implicit, we get an implicit isomorphism. */
  implicit def newTypeIso[A, B <: NewType[A]](implicit c: A => B): A <=> B =
    Iso(c, _.value)

  /** A Functor that is not necessarily an endofunctor in the Scala category. */
  trait GeneralizedFunctor[C[_,_], D[_,_], F[_]] {
    def fmap[A, B](f: C[A, B]): D[F[A], F[B]]
  }

  def endoFunctorInScala[F[_]](f: Functor[F]): GeneralizedFunctor[Function1, Function1, F] =
    new GeneralizedFunctor[Function1, Function1, F] {
      def fmap[A, B](h: A => B): F[A] => F[B] = 
        f.fmap(_, h)
    }

  def cofunctorInScala[F[_]](f: Cofunctor[F]): GeneralizedFunctor[<=, Function1, F] =
    new GeneralizedFunctor[<=, Function1, F] {
      def fmap[A, B](h: A <= B): F[A] => F[B] =
        f.comap(_, h.value)
    }

  trait GeneralizedCofunctor[C[_,_], D[_,_], F[_]] {
    def comap[A, B](f: C[A, B]): D[F[B], F[A]]
  }

  implicit def opCoFunctor[R]: Cofunctor[({type λ[α]=R <= α})#λ] =
    new Cofunctor[({type λ[α]=R <= α})#λ] {
      def comap[A, B](b: R <= A, t: B => A): R <= B =
        <=(b.value compose t)
    }

  /** Functor composition */
  case class Compose[F[_], G[_], Arr[_,_], X](value: F[G[X]]) extends NewType[F[G[X]]]

  trait <*>[F[_], G[_]] {
    trait In[A[_,_]] {
      type Apply[X] = Compose[F, G, A, X]
    }
  }

  /** Compose functors */
  implicit def ComposeFunctors[F[_]:Functor, G[_]:Functor]: Functor[(F <*> G)#In[Function1]#Apply] =
    new Functor[(F <*> G)#In[Function1]#Apply] {
      def fmap[A, B](a: Compose[F, G, Function1, A], f: A => B): Compose[F, G, Function1, B] =
        Compose(a.value map (_ map f))
    }

  /** Compose cofunctors */
  implicit def ComposeCofunctors[F[_]:Cofunctor, G[_]:Cofunctor]: Functor[(F <*> G)#In[<=]#Apply] =
    new Functor[(F <*> G)#In[<=]#Apply] {
      def fmap[A, B](a: Compose[F, G, <=, A], f: A => B): Compose[F, G, <=, B] =
        Compose(a.value comap (_ comap f))
    }

  /** Generalized natural transformations */
  trait Nat[Arr[_,_], F[_], G[_]] {
    type Apply[A] = Arr[F[A], G[A]]
  }

  type Alpha[Arr[_,_], X, Y] = ({type λ[α]=Arr[α, X]})#λ ~> ({type λ[α]=Arr[α, Y]})#λ


  /** The Yoneda Lemma 
  def yoneda[Arr[_,_]:Category, X, Y]: Iso[Function1, Alpha[Arr, X, Y], Arr[X, Y]] = {
    def to(alpha: Alpha[Arr, X, Y]): Arr[X, Y] = alpha(implicitly[Category[Arr]].id)
    def from(f: Arr[X, Y]): Alpha[Arr, X, Y] = new Alpha[Arr, X, Y] {
      def apply[A](a: => Arr[A, X]) = f <<< a
    }
    Iso(to, from)
  }
*/

  /** Fully faithful functors reflect isomorphisms */
  def reflectIso[A1[_,_], A2[_,_], F[_], A, B](implicit c1: Category[A1], c2: Category[A2], f: GeneralizedFunctor[A2, A1, F]):
    (On[A1, F]#Apply <~~> A2) => Iso[A1, F[A], F[B]] => Iso[A2, A, B] = 
      (iso => { case Iso(to, from) => Iso(iso.to(to), iso.to(from)) })

  type GeneralAdjunction[P[_,_], Q[_,_], F[_], U[_]] = Biff[P, F, Id]#Apply <~~> Biff[Q, Id, U]#Apply

  type Adjunction[F[_], U[_]] = GeneralAdjunction[Function1, Function1, F, U]

  trait Reader[R] {
    type Apply[S] = R => S
  }

  trait Writer[R] {
    type Apply[S] = (R, S)
  }

  /** The adjunction induced by curry and uncurry being isomorphic */

  def stateAdjunction[S]: Adjunction[Writer[S]#Apply, Reader[S]#Apply] = error("crap")
/*
    Iso3[~~>, Biff[Function1, Writer[S]#Apply, Id]#Apply, Biff[Function1, Id, Reader[S]#Apply]#Apply](
      new (Biff[Function1, Writer[S]#Apply, Id]#Apply ~~> Biff[Function1, Id, Reader[S]#Apply]#Apply) {
        def apply[A,B](f: => ((S, A)) => B): A => S => B = 
          a => s => f.apply((s, a))
      }, new (Biff[Function1, Id, Reader[S]#Apply]#Apply ~~> Biff[Function1, Writer[S]#Apply, Id]#Apply) {
        def apply[A,B](f: => A => S => B): ((S, A)) => B = 
          p => f.apply(p._2)(p._1)
      })
*/

  implicit def PartialFunctionCategory: Category[PartialFunction] = new Category[PartialFunction] {
    def id[A] = {case a => a}
    def compose[X, Y, Z](f: PartialFunction[Y, Z], g: PartialFunction[X, Y]) = new PartialFunction[X, Z] {
      def isDefinedAt(x: X) = g.isDefinedAt(x) && f.isDefinedAt(g(x))
      def apply(x: X) = f(g(x))
    }
  }

  implicit def KleisliCategory[M[_]: Monad]: Category[({type λ[α, β]=Kleisli[M, α, β]})#λ] = new Category[({type λ[α, β]=Kleisli[M, α, β]})#λ] {
    def id[A] = ☆(_ η)
    def compose[X, Y, Z](f: Kleisli[M, Y, Z], g: Kleisli[M, X, Y]) = f <=< g
  }

  implicit def CokleisliCategory[M[_]: Comonad]: Category[({type λ[α, β]=Cokleisli[M, α, β]})#λ] = new Category[({type λ[α, β]=Cokleisli[M, α, β]})#λ] {
    def id[A] = ★(_ ε)
    def compose[X, Y, Z](f: Cokleisli[M, Y, Z], g: Cokleisli[M, X, Y]) = f =<= g 
  }

  /* Every monoid gives rise to a category 
  implicit def MonoidCategory[M: Monoid] = new Category[PartialApply1Of3[Const2,M]#Apply] {
    def id[A] = Const2(implicitly[Zero[M]].zero)
    def compose[X, Y, Z](f: Const2[M, Y, Z], g: Const2[M, X, Y]) = Const2(f.value |+| g.value)
  }
  */

  implicit def ObjectToMorphism[A, B, C](a: A): Const2[A, Unit, Unit] = Const2(a)
  implicit def MorphismToObject[A, B, C](a: Const2[A, B, C]) = a.value

  case class Discrete[X, A, B](value: X => X) extends NewType[X => X]

  /** Discrete categories, whose only morphism is the identity function. **/
  implicit def DiscreteCategory[X] = new Category[({type λ[α, β]=Discrete[X, α, β]})#λ] {
    def id[A] = Discrete(x => x)
    def compose[A,B,C](f: Discrete[X, B, C], g: Discrete[X, A, B]) = Discrete(f.value compose g.value)
  }

  sealed class Ord2[X, A, B](implicit o: Order[X]) {
    def compare(a: X, b: X) = a lte b
  }

  /** Every partial order gives rise to a category **/
  implicit def PosetCategory[X: Order]: Category[({type λ[α, β]=Ord2[X, α, β]})#λ] = new Category[({type λ[α, β]=Ord2[X, α, β]})#λ] {
    def id[A] = new Ord2[X, A, A]
    def compose[A, B, C](f: Ord2[X, B, C], g: Ord2[X, A, B]) = new Ord2[X, A, C] {
      override def compare(a: X, b: X) = f.compare(a, b) == g.compare(a, b)
    } 
  }
}
