package scalaz

import scala.language.experimental.macros

sealed abstract class Isomorphisms {

  /**Isomorphism for arrows of kind * -> * -> * */
  trait Iso[Arr[_, _], A, B] {
    self =>
    def to: Arr[A, B]
    def from: Arr[B, A]
    def flip: Iso[Arr, B, A] = new Iso[Arr, B, A] {
      val to = self.from
      val from = self.to
      override def flip = self
    }

    def %~(f: Arr[B, B])(implicit C: Compose[Arr]): Arr[A, A] =
      C.compose(from, C.compose(f, to))
  }

  /**Isomorphism for arrows of kind (* -> *) -> (* -> *) -> * */
  trait Iso2[Arr[_[_], _[_]], F[_], G[_]] {
    self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def flip: Iso2[Arr, G, F] = new Iso2[Arr, G, F] {
      val to = self.from
      val from = self.to
      override def flip = self
    }

    import Liskov._

    def unlift[A](implicit FG: Arr[F, G] <~< (F ~> G), GF: Arr[G, F] <~< (G ~> F)): F[A] <=> G[A] =
      new (F[A] <=> G[A]){
        def from = GF(self.from).apply
        def to   = FG(self.to).apply
      }

    def %~(f: G ~> G)(implicit FG: Arr[F, G] <~< (F ~> G), GF: Arr[G, F] <~< (G ~> F)): F ~> F =
      new (F ~> F) {
        def apply[A](a: F[A]): F[A] = GF(self.from)(f(FG(self.to)(a)))
      }
  }

  /**Isomorphism for arrows of kind (* -> * -> *) -> (* -> * -> *) -> * */
  trait Iso3[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] {
    self =>
    def to: Arr[F, G]
    def from: Arr[G, F]
    def flip: Iso3[Arr, G, F] = new Iso3[Arr, G, F] {
      val to = self.from
      val from = self.to
      override def flip = self
    }

    import Liskov._

    def unlift[A, B](implicit
      FG: Arr[F, G] <~< (F ~~> G),
      GF: Arr[G, F] <~< (G ~~> F)
    ): F[A, B] <=> G[A, B] =
      new (F[A, B] <=> G[A, B]){
        def from = GF(self.from).apply _
        def to   = FG(self.to).apply _
      }

    def unlift1[A](implicit
      FG: Arr[F, G] <~< (F ~~> G),
      GF: Arr[G, F] <~< (G ~~> F)
    ): F[A, *] <~> G[A, *] = {
      type FA[α] = F[A, α]
      type GA[α] = G[A, α]
      new IsoFunctorTemplate[FA, GA]{
        def from_[X](ga: GA[X]) = GF(self.from)(ga)
        def to_[X](fa: FA[X]) = FG(self.to)(fa)
      }
    }

    def unlift2[A](implicit
      FG: Arr[F, G] <~< (F ~~> G),
      GF: Arr[G, F] <~< (G ~~> F)
    ): F[*, A] <~> G[*, A] = {
      type FA[α] = F[α, A]
      type GA[α] = G[α, A]
      new IsoFunctorTemplate[FA, GA]{
        def from_[X](ga: GA[X]) = GF(self.from)(ga)
        def to_[X](fa: FA[X]) = FG(self.to)(fa)
      }
    }

    def %~(f: G ~~> G)(implicit FG: Arr[F, G] <~< (F ~~> G), GF: Arr[G, F] <~< (G ~~> F)): F ~~> F =
      new (F ~~> F) {
        def apply[A, B](a: F[A, B]): F[A, B] = GF(self.from)(f(FG(self.to)(a)))
      }
  }

  /**Set isomorphism */
  type IsoSet[A, B] = Iso[Function1, A, B]

  /**Natural isomorphism between functors */
  type IsoFunctor[F[_], G[_]] = Iso2[NaturalTransformation, F, G]

  type IsoBifunctor[F[_, _], G[_, _]] = Iso3[~~>, F, G]

  /**Alias for IsoSet */
  type <=>[A, B] = IsoSet[A, B]

  object IsoSet {
    /**Convenience constructor to implement `A <=> B` from `A => B` and `B => A` */
    def apply[A, B](to: A => B, from: B => A): A <=> B = {
      val _to   = to
      val _from = from
      new Iso[Function1, A, B] {
        override val to   = _to
        override val from = _from
      }
    }
  }

  /**Alias for IsoFunctor */
  type <~>[F[_], G[_]] = IsoFunctor[F, G]

  /**Convenience template trait to implement `<~>` */
  trait IsoFunctorTemplate[F[_], G[_]] extends IsoFunctor[F, G] {
    override final val to: NaturalTransformation[F, G] = new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = to_[A](fa)
    }
    override final val from: NaturalTransformation[G, F] = new (G ~> F) {
      def apply[A](ga: G[A]): F[A] = from_[A](ga)
    }

    def to_[A](fa: F[A]): G[A]
    def from_[A](ga: G[A]): F[A]
  }

  object IsoFunctor {
    /**Convenience constructor to implement `F <~> G` from F ~> G and G ~> F */
    def apply[F[_], G[_]](to: F ~> G, from: G ~> F): F <~> G = {
      val _to   = to
      val _from = from
      new (F <~> G) {
        override val to = _to
        override val from = _from
      }
    }
  }

  /**Alias for IsoBifunctor */
  type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]

  /**Convenience template trait to implement `<~~>` */
  trait IsoBifunctorTemplate[F[_, _], G[_, _]] extends IsoBifunctor[F, G] {
    final val to: BiNaturalTransformation[F, G] = new (F ~~> G) {
      def apply[A, B](fab: F[A, B]): G[A, B] = to_[A, B](fab)
    }
    final val from: BiNaturalTransformation[G, F] = new (G ~~> F) {
      def apply[A, B](gab: G[A, B]): F[A, B] = from_[A, B](gab)
    }

    def to_[A, B](fa: F[A, B]): G[A, B]
    def from_[A, B](ga: G[A, B]): F[A, B]
  }

  /**Set isomorphism is commutative */
  def commutative[A, B](i: A <=> B): B <=> A = i.flip

  /**Set isomorphism is reflexive */
  def refl[A]: A <=> A = new (A <=> A) {
    def to: A => A = a => a
    def from: A => A = a => a
  }

  /**Natural isomorphism is reflexive */
  def naturalRefl[F[_]]: F <~> F = new IsoFunctorTemplate[F, F] {
    def to_[A](fa: F[A]): F[A] = fa
    def from_[A](fa: F[A]): F[A] = fa
  }

  /**Natural isomorphism is commutative */
  def naturalCommutative[F[_], G[_]](i: F <~> G): G <~> F = i.flip

}

object Isomorphism extends Isomorphisms {
  implicit def genProd[CC <: Product, T <: Product]: Isomorphism.Iso[Function1, CC, T] = macro IsomorphismMacro.genProdImpl[CC, T]
  implicit def genCop[ST <: AnyRef, OO <: OneOf]: Isomorphism.Iso[Function1, ST, OO] = macro IsomorphismMacro.genCopImpl[ST, OO]
}

import Isomorphism._

trait IsomorphismAssociative[F[_, _], G[_, _]] extends Associative[F] {
  implicit def G: Associative[G] with Bifunctor[G] // TODO: is this needed? (I think so)

  def iso: F <~~> G

  override def reassociateLeft[A, B, C](f: F[A, F[B, C]]): F[F[A, B], C] =
    iso.from(G.leftMap(G.reassociateLeft(G.rightMap(iso.to(f))(iso.to.apply _)))(iso.from.apply _))

  override def reassociateRight[A, B, C](f: F[F[A, B], C]): F[A, F[B, C]] =
    iso.from(G.rightMap(G.reassociateRight(G.leftMap(iso.to(f))(iso.to.apply _)))(iso.from.apply _))
}

object IsomorphismMacro {
  // TODO is there a way to do this with blackbox? Things don't get materialised without whitebox
  import scala.reflect.macros.whitebox.Context

  // Copyright 2020 Daniel Vigovszky (vigoo)
  def genProdImpl[Prod: c.WeakTypeTag, Tup: c.WeakTypeTag](c: Context): c.Expr[Isomorphism.Iso[Function1, Prod, Tup]] = {
    import c.universe._

    val prodTpe = c.weakTypeOf[Prod]

    if (!prodTpe.typeSymbol.isClass ||
      !prodTpe.typeSymbol.asClass.isCaseClass) {
      c.abort(c.enclosingPosition, s"Type ${prodTpe.typeSymbol} is not a case class")
    }

    val paramLists = prodTpe.typeSymbol.asClass.primaryConstructor.asMethod.typeSignatureIn(prodTpe).paramLists
    val result = paramLists match {
      case List(params) =>
        val tupleName = definitions.TupleClass(params.size).name.toTypeName
        val tupleParams = params.map { sym =>
          val symLit = Literal(Constant(sym.name.toString))
          tq"_root_.scalaz.@@[${sym.typeSignatureIn(prodTpe).finalResultType}, $symLit]"
        }
        val tup = tq"$tupleName[..$tupleParams]"
        val packers =
          params.map { sym =>
            val symLit = Literal(Constant(sym.name.toString))
            val symTerm = sym.name.toTermName
            q"_root_.scalaz.Tag.apply[${sym.typeSignatureIn(prodTpe).finalResultType}, $symLit](a.$symTerm)"
          }

        val unpackers =
          params.indices.map { idx =>
            val accessor = TermName(s"_${idx+1}")
            q"b.$accessor.value"
          }

        q"""new _root_.scalaz.Isomorphism.Iso[Function1, $prodTpe, $tup] {
              override val to: ($prodTpe => $tup) = a => (..$packers)
              override val from: ($tup => $prodTpe) = b => ${prodTpe.typeSymbol.companion}.apply(..$unpackers)
            }"""

      case Nil =>
        q"""new _root_.scalaz.Isomorphism.Iso[Function1, $prodTpe, Unit] {
              override val to: ($prodTpe => Unit) = _ => ()
              override val from: (Unit => $prodTpe) = _ => $prodTpe()
            }"""

      case _ => c.abort(c.enclosingPosition, s"Type ${prodTpe.typeSymbol} has multiple parameter lists which is currently not supported")
    }

    println(result)
    c.Expr(result)
  }

  // NOTE these don't have tagged types because that works ok via other type name providers
  def genCopImpl[ST: c.WeakTypeTag, OO: c.WeakTypeTag](c: Context): c.Expr[Isomorphism.Iso[Function1, ST, OO]] = {
    import c.universe._

    val ST = c.weakTypeOf[ST]
    val cls = ST.typeSymbol.asClass

    if (!cls.isSealed)
      c.abort(c.enclosingPosition, s"${ST.typeSymbol} is not a sealed trait")

    // ordering is ill-defined, we use source ordering
    val parts =
        cls.knownDirectSubclasses.toList
          .map(_.asClass)
          .sortBy(_.pos.start)
          .map { cl =>
            if (cl.isModuleClass)
              internal.singleType(cl.thisPrefix, cl.module)
            else {
              val t    = cl.toType
              val args = t.typeArgs.map { a =>
                val sym  = a.typeSymbol
                val tSym = ST
                  .find(_.typeSymbol.name == sym.name)
                  .getOrElse(
                    c.abort(
                      c.enclosingPosition,
                      s"type parameters on case classes ($t[${t.typeArgs}]) are not supported unless they are on the sealed trait ($ST)"
                    )
                  )
                a.substituteTypes(List(sym), List(tSym))
              }
              appliedType(t, args)
            }
          }

    val oneof_cls = c.mirror.staticClass(s"_root_.scalaz.OneOf${parts.length}")
    val oneof = appliedType(oneof_cls, parts) // == OO.typeSymbol.asClass
    // TODO staticClass things are printed without the FQN, but should have them

    val to_matchers = parts.zipWithIndex.map {
      case (tp, i) =>
        val cons = c.mirror.staticClass(s"_root_.scalaz.OneOf${parts.length}.V${i + 1}")
        cq"p : $tp => ${cons.companion}.apply(p)"
    }

    val from_matchers = parts.zipWithIndex.map {
      case (_, i) =>
        val uncons = c.mirror.staticClass(s"_root_.scalaz.OneOf${parts.length}.V${i + 1}")
        cq"${uncons.companion}(p) => p"
    }

    val result =
      q"""new _root_.scalaz.Isomorphism.Iso[Function1, $ST, $oneof] {
            override val to: ($ST => $oneof) = a => a match { case ..$to_matchers }
            override val from: ($oneof => $ST) = b => b match { case ..$from_matchers }
          }"""

    c.Expr(result)
  }

}
