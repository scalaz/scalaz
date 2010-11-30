package scalaz

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

trait GeneralizedCategory[L,H>:L] {
  // Type member used instead of type parameter to work around http://lampsvn.epfl.ch/trac/scala/ticket/4043
  type =>:[_ >: L <: H, _ >: L <: H]

  def id[A>:L<:H]: A =>: A
  def compose[A>:L<:H, B>:L<:H, C>:L<:H](f: B =>: C, g: A =>: B): A =>: C

/*
  def *[LY,HY>:LY,Y[_>:LY<:HY,_>:LY<:HY]](
    that : GeneralizedCategory[LY,HY] { type =>:[A>:LY<:HY,B>:LY<:HY] = Y[A,B]; }
  ) = Category.Product[L,H,=>:,LY,HY,Y](this,that)
*/
}

trait GeneralizedGroupoid[L,H>:L] extends GeneralizedCategory[L,H] { 
  def invert[A>:L<:H,B>:L<:H](f : A =>: B): B =>: A
}

trait Category[X[_, _]] extends GeneralizedCategory[Nothing,Any] {
  type =>:[A, B] = X[A, B]
}

trait Groupoid[X[_, _]] extends GeneralizedGroupoid[Nothing,Any] with Category[X]

object Category {
  import Scalaz._
  import Leibniz._

  /* 
   * Product Categories
   */

  /** Index for a product category. This is a pair enriched with its member types */
  case class <>[+IX,+IY](_1: IX, _2: IY) { type _1 = IX; type _2 = IY }

  abstract sealed class Product[
    LX,HX>:LX,X[_>:LX<:HX,_>:LX<:HX], // left category
    LY,HY>:LY,Y[_>:LY<:HY,_>:LY<:HY], // right category
    A>:LX<>LY<:HX<>HY, // from
    B>:LX<>LY<:HX<>HY  // to
  ]{
    def pair[AX>:LX<:HX, AY>:LY<:HY, BX>:LX<:HX, BY>:LY<:HY](
      implicit a: A === (AX<>AY), b: B === (BX<>BY)
    ):(X[AX,BX],Y[AY,BY])
  }

  implicit def Product[LX,HX>:LX,X[_>:LX<:HX,_>:LX<:HX],LY,HY>:LY,Y[_>:LY<:HY,_>:LY<:HY]] (
    implicit x: GeneralizedCategory[LX,HX] { type =>:[A>:LX<:HX,B>:LX<:HX]=X[A,B]},
             y: GeneralizedCategory[LY,HY] { type =>:[A>:LY<:HY,B>:LY<:HY]=Y[A,B]}
  ) : GeneralizedCategory[LX<>LY,HX<>HY] = new GeneralizedCategory[LX<>LY,HX<>HY] {
    type L = LX<>LY
    type H = HX<>HY
    type =>:[A>:L<:H,B>:L<:H] = Product[LX,HX,X,LY,HY,Y,A,B]

    def id[A>:L<:H] = new Product[LX,HX,X, LY,HY,Y, A, A] {
      def pair[AX>:LX<:HX,AY>:LY<:HY,BX>:LX<:HX,BY>:LY<:HY](
        implicit a: A === (AX<>AY), b: A === (BX<>BY)
      ) : (X[AX,BX], Y[AY,BY]) = (x.id[AX], y.id[AY]).asInstanceOf[(X[AX,BX], Y[AY,BY])]
    }
    def compose [A>:L<:H, B>:L<:H, C>:L<:H](
      f: Product[LX,HX,X, LY,HY,Y, B, C],
      g: Product[LX,HX,X, LY,HY,Y, A, B]
    ) = new Product[LX,HX,X, LY,HY,Y, A, C] {
      def pair[AX>:LX<:HX,AY>:LY<:HY,CX>:LX<:HX,CY>:LY<:HY](
        implicit a: A ===  (AX<>AY), c: C === (CX<>CY)
      ) : (X[AX,CX], Y[AY,CY]) = {
	def go[BX>:LX<:HX,BY >:LY<:HY](b : B === (BX<>BY)): (X[AX,CX], Y[AY,CY]) = {
          val (fx,fy) = f.pair(b, c)
          val (gx,gy) = g.pair(a, b)
	  (x.compose[AX,BX,CX](fx,gx), y.compose[AY,BY,CY](fy, gy))
	}
	go[B#_1,B#_2](force[Nothing, Any, B, B#_1 <> B#_2])
      }
    }
  }

  implicit def productToPair[
    LX, HX>:LX, X[_>:LX<:HX,_>:LX<:HX], 
    LY, HY>:LY, Y[_>:LY<:HY,_>:LY<:HY], 
    AX>:LX<:HX, BX>:LX<:HX, AY>:LY<:HY, BY>:LY<:HY
  ](
    p: Product[LX,HX,X, LY,HY,Y, AX<>AY, BX<>BY]
  ) = p.pair[AX, AY, BX, BY]

  implicit def pairToProduct[
    LX, HX>:LX, X[_>:LX<:HX,_>:LX<:HX],
    LY, HY>:LY, Y[_>:LY<:HY,_>:LY<:HY],
    AX>:LX<:HX, BX>:LX<:HX, AY>:LY<:HY, BY>:LY<:HY
  ](
    p: (X[AX,BX], Y[AY,BY])
  ) : Product[LX,HX,X, LY,HY,Y, AX<>AY, BX<>BY] =
  new Product[LX,HX,X, LY,HY,Y, AX<>AY, BX<>BY] {
    def pair[AX_ >:LX<:HX, AY_ >:LY<:HY, BX_ >:LX<:HX, BY_ >:LY<:HY](
      implicit a: (AX<>AY) === (AX_ <> AY_), 
               b: (BX<>BY) === (BX_ <> BY_)
    ) : (X[AX_,BX_], Y[AY_,BY_]) = p.asInstanceOf[(X[AX_,BX_],Y[AY_,BY_])]
  }

  trait PartialApplyProduct[LX,HX>:LX,X[_>:LX<:HX,_>:LX<:HX], LY,HY>:LY,Y[_>:LY<:HY,_>:LY<:HY]] {
    type L = LX <> LY
    type H = HX <> HY
    type Apply[A>:L<:H, B>:L<:H] = Product[LX,HX,X,LY,HY,Y,A,B]
  }

  /*
   * The category given by a monoid
   */

  trait Mon[M] {
    type Apply[A,B] = M
  }

  implicit def MonoidCategory[M](monoid : Monoid[M]) = new GeneralizedCategory[Nothing,Nothing] { 
    type =>:[A,B] = Mon[M]#Apply[A,B]
    def id[A] = monoid.zero
    def compose[A,B,C](m: M, n : M) : M = monoid.append(m, n)
  }
  
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
    def compose[X,Y,Z](f: Y <= Z, g: X <= Y): X <= Z =
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
  implicit def ComposeCoFunctors[F[_]:Cofunctor, G[_]:Cofunctor]: Functor[(F <*> G)#In[<=]#Apply] =
    new Functor[(F <*> G)#In[<=]#Apply] {
      def fmap[A, B](a: Compose[F, G, <=, A], f: A => B): Compose[F, G, <=, B] =
        Compose(a.value comap (_ comap f))
    }

  /** Generalized natural transformations */
  trait Nat[Arr[_,_], F[_], G[_]] {
    type Apply[A] = Arr[F[A], G[A]]
  }

  type Alpha[Arr[_,_], X, Y] = ({type λ[α]=Arr[α, X]})#λ ~> ({type λ[α]=Arr[α, Y]})#λ

  /** The Yoneda Lemma */
  def yoneda[Arr[_,_]:Category, X, Y]: Iso[Function1, Alpha[Arr, X, Y], Arr[X, Y]] = {
    def to(alpha: Alpha[Arr, X, Y]): Arr[X, Y] = alpha(implicitly[Category[Arr]].id)
    def from(f: Arr[X, Y]): Alpha[Arr, X, Y] = new Alpha[Arr, X, Y] {
      def apply[A](a: => Arr[A, X]) = f <<< a
    }
    Iso(to, from)
  }

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
  def stateAdjunction[S]: Adjunction[Writer[S]#Apply, Reader[S]#Apply] =
    Iso3[~~>, Biff[Function1, Writer[S]#Apply, Id]#Apply, Biff[Function1, Id, Reader[S]#Apply]#Apply](
      new (Biff[Function1, Writer[S]#Apply, Id]#Apply ~~> Biff[Function1, Id, Reader[S]#Apply]#Apply) {
        def apply[A,B](f: => ((S, A)) => B): A => S => B = 
          a => s => f.apply((s, a))
      }, new (Biff[Function1, Id, Reader[S]#Apply]#Apply ~~> Biff[Function1, Writer[S]#Apply, Id]#Apply) {
        def apply[A,B](f: => A => S => B): ((S, A)) => B = 
          p => f.apply(p._2)(p._1)
      })

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

  implicit def ObjectToMorphism[A,B,C](a: A): Const2[A,Unit,Unit] = Const2(a)
  implicit def MorphismToObject[A,B,C](a: Const2[A,B,C]) = a.value

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
  implicit def PosetCategory[X: Order] = new Category[({type λ[α, β]=Ord2[X, α, β]})#λ] {
    def id[A] = new Ord2[X, A, A]
    def compose[A, B, C](f: Ord2[X, B, C], g: Ord2[X, A, B]) = new Ord2[X, A, C] {
      override def compare(a: X, b: X) = f.compare(a, b) == g.compare(a, b)
    } 
  }
}
