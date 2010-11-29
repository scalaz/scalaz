package scalaz

trait GeneralizedCategory {
  type L
  type H >: L
  type ~>[_ >: L <: H, _ >: L <: H]

  def id[A>:L<:H]: A ~> A
  def compose[A>:L<:H, B>:L<:H, C>:L<:H](f: B ~> C, g: A ~> B): A ~> C
}


object GeneralizedCategory {
  import Leibniz._

  trait Category[X[_, _]] extends GeneralizedCategory {
    type L = Nothing
    type H = Any
    type ~>[A, B] = X[A, B]
  }

  implicit def function1 = new Category[Function1] {
    def id[A] = x => x
    def compose[A,B,C](f: B => C, g: A => B) = f compose g
  }

  case class P[+IX,+IY](_1: IX, _2: IY) { type _1 = IX; type _2 = IY }
  implicit def injectiveP = Injective2[P]

  trait Product[LX<:HX,HX>:LX, X[_>:LX<:HX,_>:LX<:HX], LY<:HY, HY>:LY, Y[_>:LY<:HY,_>:LY<:HY], A>:P[LX,LY]<:P[HX,HY], B>:P[LX,LY]<:P[HX,HY]] {
    def pair[AX>:LX<:HX, AY>:LY<:HY, BX>:LX<:HX, BY>:LY<:HY](
      implicit a: A ~ P[AX,AY], b: B ~ P[BX,BY]
    ) : (X[AX,BX], Y[AY,BY])
  }

  implicit def productToPair[LX<:HX, HX>:LX, X[_>:LX<:HX,_>:LX<:HX], LY<:HY, HY>:LY, Y[_>:LY<:HY,_>:LY<:HY], AX>:LX<:HX, BX>:LX<:HX, AY>:LY<:HY, BY>:LY<:HY](
    p: Product[LX,HX,X, LY,HY,Y, P[AX,AY], P[BX,BY]]
  ) = p.pair[AX, AY, BX, BY]

  implicit def pairToProduct[LX<:HX, HX>:LX, X[_>:LX<:HX,_>:LX<:HX], LY<:HY, HY>:LX, Y[_>:LY<:HY,_>:LY<:HY], AX>:LX<:HX, BX>:LX<:HX, AY>:LY<:HY, BY>:LY<:HY](
    p: (X[AX,BX], Y[AY,BY])
  ) : Product[LX,HX,X, LY,HY,Y, P[AX,AY], P[BX,BY]] =
  new Product[LX,HX,X, LY,HY,Y, P[AX,AY], P[BX,BY]] {
    def pair[AX_ >:LX<:HX, AY_ >:LY<:HY, BX_ >:LX<:HX, BY_ >:LY<:HY](
      implicit a: P[AX,AY] ~ P[AX_,AY_], b: P[BX,BY] ~ P[BX_,BY_]
    ) = {
      val (a1,a2) = lower2[P, AX, AX_, AY, AY_](a)
      val (b1,b2) = lower2[P, BX, BX_, BY, BY_](b)
      ( force(p._1), force(p._2) )
      // ( liftB2[X,LX,HX,LY,HY,AX,AX_,BX,BX_](a1,b1)(p._1), liftB2[Y,LX,HX,LY,HY,AY,AY_,BY,BY_](a2,b2)(p._2))
    }
  }

  trait PartialApplyProduct[LX<:HX,HX>:LX,X[_>:LX<:HX,_>:LX<:HX], LY<:HY,HY>:LY,Y[_>:LY<:HY,_>:LY<:HY]] {
    type Apply[A>:P[LX,LY]<:P[HX,HY], B>:P[LX,LY]<:P[HX,HY]] = Product[LX,HX,X, LY,HY,Y, A, B]
  }


  implicit def product[LX<:HX,HX>:LX,X[_>:LX<:HX,_>:LX<:HX],LY<:HY,HY>:LY,Y[_>:LY<:HY,_>:LY<:HY]] (
    implicit x: GeneralizedCategory {type L = LX; type H = HX; type ~>[A >: L <: H, B >: L <: H] = X[A, B]},
             y: GeneralizedCategory {type L = LY; type H = HY; type ~>[A >: L <: H, B >: L <: H] = Y[A, B]}
  ) =
  new GeneralizedCategory {
    type L = P[LX,LY]
    type H = P[HX,HY]
    type ~>[A >: L <: H, B >: L <: H] = Product[LX, HX, X, LY, HY, Y, A, B]

    def id[A>:P[LX,LY]<:P[HX,HY]] = new Product[LX,HX,X, LY,HY,Y, A, A] {
      def pair[AX>:LX<:HX,AY>:LY<:HY,BX>:LX<:HX,BY>:LY<:HY](implicit a: A ~ P[AX,AY], b: A ~ P[BX,BY]) : (X[AX,BX],Y[AY,BY]) = {
        val (axbx, ayby) = lower2[P,AX,BX,AY,BY](b compose a.inverse)
        error("TODO")
        ( force(x.id[AX]), force(y.id[AY]) )
        //(witness(lift2[X,AX,AX,AX,BX](refl, axbx))(x.id[AX]), witness(lift2[Y,AY,AY,AY,BY](refl, ayby))(y.id[AY]))
      }
    }
    def compose [A>:P[LX,LY]<:P[HX,HY], B>:P[LX,LY]<:P[HX,HY], C>:P[LX,LY]<:P[HX,HY]](
      f: Product[LX,HX,X, LY,HY,Y, B, C],
      g: Product[LX,HX,X, LY,HY,Y, A, B]
    ) = new Product[LX,HX,X, LY,HY,Y, A, C] {
      def pair[AX>:LX<:HX,AY>:LY<:HY,CX>:LX<:HX,CY>:LY<:HY](
        implicit a: A ~ P[AX,AY],
                 c: C ~ P[CX,CY]
      ) : (X[AX,CX], Y[AY,CY]) = {
	def go[BX>:LX<:HX,BY >:LY<:HY](b : B ~ P[BX,BY]): (X[AX,CX], Y[AY,CY]) = {
          val (fx,fy) = f.pair(b, c)
          val (gx,gy) = g.pair(a, b)
	  (x.compose[AX,BX,CX](fx,gx), y.compose[AY,BY,CY](fy, gy))
	}
	go[B#_1,B#_2](force[B,P[B#_1,B#_2]])
      }
    }
  }

  trait Mon[M] {
    type Apply[A,B] = M
  }

  implicit def monoid[M](monoid : Monoid[M]) = new GeneralizedCategory { 
    type L = Nothing
    type H = Nothing
    type ~>[A,B] = Mon[M]#Apply[A,B]
    def id[A] = monoid.zero
    def compose[A,B,C](m: M, n : M) : M = monoid.append(m, n)
  }
}
