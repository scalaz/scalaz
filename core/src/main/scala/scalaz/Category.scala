package scalaz

trait Category[F[_, _]] {
  val id: Id[F]
  val compose: Compose[F]

  def i[A]: F[A, A] =
    id.id[A]

  def comp[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    compose.compose(f, g)
}

object Category extends Categorys

trait Categorys {
  def category[F[_, _]](implicit ii: Id[F], c: Compose[F]): Category[F] = new Category[F] {
    val id = ii
    val compose = c
  }

  implicit val Function1Category: Category[Function1] =
    category[Function1]

  implicit val PartialFunctionCategory: Category[PartialFunction] =
    category[PartialFunction]

  implicit val `<:<_Category` : Category[<:<] =
    category[<:<]

  implicit val `=:=_Category` : Category[=:=] =
    category[=:=]

  implicit def opContravariant[R]: Contravariant[({type λ[α]=R <= α})#λ] =
    new Contravariant[({type λ[α]=R <= α})#λ] {
      def contramap[A, B](b: R <= A, t: B => A): R <= B =
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

  /** Compose contravariants */
  implicit def ComposeContravariants[F[_]:Contravariant, G[_]:Contravariant]: Functor[(F <*> G)#In[<=]#Apply] =
    new Functor[(F <*> G)#In[<=]#Apply] {
      def fmap[A, B](a: Compose[F, G, <=, A], f: A => B): Compose[F, G, <=, B] =
        Compose(a.value contramap (_ contramap f))
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
    def id[A] = ★(_ copure)
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
