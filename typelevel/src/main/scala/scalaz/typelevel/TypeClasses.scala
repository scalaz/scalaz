package scalaz
package typelevel

import Typelevel._

trait TypeClass[C[_[_]]] {

  import TypeClass._

  def product[F[_], T <: TCList](FHead: C[F], FTail: C[T#λ]): C[TCCons[F, T]#λ]
  def compose[F[_], T <: TCList](FOuter: C[F], FInner: C[T#Composed]): C[TCCons[F, T]#Composed]

  def _emptyProduct: C[TCNil#λ]
  def _idCompose: C[TCNil#Composed]

  final def emptyProduct = new WrappedProduct[C, TCNil](_emptyProduct, this)
  final def idCompose = new WrappedComposition[C, TCNil](_idCompose, this)

}


object TypeClass {

  def apply[C[_[_]]](implicit C: TypeClass[C]) = C

  trait Empty {
    def _emptyProduct: Applicative[TCNil#λ] = new Applicative[TCNil#λ] {
      override def map[A, B](fa: HNil)(f: A => B): HNil = HNil
      def point[A](a: => A): HNil = HNil
      def ap[A, B](fa: => HNil)(f: => HNil): HNil = HNil
    }

    def _idCompose: Applicative[TCNil#Composed] = new Applicative[TCNil#Composed] {
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      def point[A](a: => A): A = a
      def ap[A, B](fa: => A)(f: => A => B): B = f(fa)
    }
  }

  // Wrappers

  class WrappedProduct[C[_[_]], T <: TCList](val instance: C[T#λ], typeClass: TypeClass[C]) {

    def *:[F[_]](F: C[F]) = new WrappedProduct[C, TCCons[F, T]](typeClass.product(F, instance), typeClass)

    def prodLeft[F[_]](F: C[F]) = F *: this

  }

  class WrappedComposition[C[_[_]], T <: TCList](val instance: C[T#Composed], typeClass: TypeClass[C]) {

    def <<:[F[_]](F: C[F]) = new WrappedComposition[C, TCCons[F, T]](typeClass.compose[F, T](F, instance), typeClass)

    def >>[F[_]](F: C[F]) = F <<: this

    def composeInto[F[_]](F: C[F]) = F <<: this

  }

  class TypeClassCompanion[C[_[_]] : TypeClass]() {

    def product = TypeClass[C].emptyProduct
    def compose = TypeClass[C].idCompose

  }

  // Products

  class ProductFunctor[F[_], T <: TCList](FHead: Functor[F], FTail: Functor[T#λ])
    extends Functor[TCCons[F, T]#λ] {

    def map[A, B](fa: F[A] :: T#λ[A])(f: A => B) =
      FHead.map(fa.head)(f) :: FTail.map(fa.tail)(f)

  }

  class ProductPointed[F[_], T <: TCList](FHead: Pointed[F], FTail: Pointed[T#λ])
    extends ProductFunctor[F, T](FHead, FTail)
    with Pointed[TCCons[F, T]#λ] {

    def point[A](a: => A) =
      FHead.point(a) :: FTail.point(a)

  }

  class ProductApplicative[F[_], T <: TCList](FHead: Applicative[F], FTail: Applicative[T#λ])
    extends ProductPointed[F, T](FHead, FTail)
    with Applicative[TCCons[F, T]#λ] {

    def ap[A, B](fa: => F[A] :: T#λ[A])(f: => F[A => B] :: T#λ[A => B]) =
      FHead.ap(fa.head)(f.head) :: FTail.ap(fa.tail)(f.tail)

  }

  // Composition

  class ComposedFunctor[F[_], T <: TCList](FOuter: Functor[F], FInner: Functor[T#Composed])
    extends Functor[TCCons[F, T]#Composed] {

    def map[A, B](fa: F[T#Composed[A]])(f: A => B) =
      FOuter.map(fa)(ia => FInner.map(ia)(f))

  }

  class ComposedPointed[F[_], T <: TCList](FOuter: Pointed[F], FInner: Pointed[T#Composed])
    extends ComposedFunctor[F, T](FOuter, FInner)
    with Pointed[TCCons[F, T]#Composed] {

    def point[A](a: => A) =
      FOuter.point(FInner.point(a))

  }

  class ComposedApplicative[F[_], T <: TCList](FOuter: Applicative[F], FInner: Applicative[T#Composed])
    extends ComposedPointed[F, T](FOuter, FInner)
    with Applicative[TCCons[F, T]#Composed] {

    def ap[A, B](fa: => F[T#Composed[A]])(f: => F[T#Composed[A => B]]) =
      FOuter.map2(f, fa)((ff, ia) => FInner.ap(ia)(ff))

  }

}

trait TypeClasses {

  // TypeClass instances

  import TypeClass._

  implicit object FunctorClass extends TypeClass[Functor] with Empty {

    def product[F[_], T <: TCList](FHead: Functor[F], FTail: Functor[T#λ]) =
      new ProductFunctor(FHead, FTail)

    def compose[F[_], T <: TCList](FOuter: Functor[F], FInner: Functor[T#Composed]) =
      new ComposedFunctor(FOuter, FInner)

  }

  implicit object PointedClass extends TypeClass[Pointed] with Empty {

    def product[F[_], T <: TCList](FHead: Pointed[F], FTail: Pointed[T#λ]) =
      new ProductPointed(FHead, FTail)

    def compose[F[_], T <: TCList](FOuter: Pointed[F], FInner: Pointed[T#Composed]) =
      new ComposedPointed(FOuter, FInner)

  }

  implicit object ApplicativeClass extends TypeClass[Applicative] with Empty {

    def product[F[_], T <: TCList](FHead: Applicative[F], FTail: Applicative[T#λ]) =
      new ProductApplicative(FHead, FTail)

    def compose[F[_], T <: TCList](FOuter: Applicative[F], FInner: Applicative[T#Composed]) =
      new ComposedApplicative(FOuter, FInner)

  }

  // Unpack wrappers

  import TypeClass._

  implicit def unpackProduct[C[_[_]], T <: TCList](wrapper: WrappedProduct[C, T]) = wrapper.instance

  implicit def unpackComposition[C[_[_]], T <: TCList](wrapper: WrappedComposition[C, T]) = wrapper.instance

  // Wrap companions

  implicit def wrapFunctor(functor: Functor.type) = new TypeClassCompanion[Functor]
  implicit def wrapPointed(pointed: Pointed.type) = new TypeClassCompanion[Pointed]
  implicit def wrapApplicative(applicative: Applicative.type) = new TypeClassCompanion[Applicative]

}

// vim: expandtab:ts=2:sw=2

