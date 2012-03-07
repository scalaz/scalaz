package scalaz
package iteratee

import effect._
import Iteratee._
import Ordering._

trait EnumeratorT[X, E, F[_]] { self =>
  def apply[A]: StepT[X, E, F, A] => IterateeT[X, E, F, A]

  def mapE[I](et: EnumerateeT[X, E, I, F])(implicit M: Monad[F]): EnumeratorT[X, I, F] = et run self

  def map[B](f: E => B)(implicit ev: Monad[F]): EnumeratorT[X, B, F] = 
    EnumerateeT.map[X, E, B, F](f) run self

  def #::(e: => E)(implicit F: Monad[F]): EnumeratorT[X, E, F] = {
    new EnumeratorT[X, E, F] {
      def apply[A] = _.mapCont(_(elInput(e))) &= self
    }
  }

  def flatMap[B](f: E => EnumeratorT[X, B, F])(implicit M1: Monad[F]) = 
    EnumerateeT.flatMap(f) run self

  def flatten[B, G[_]](implicit ev: E =:= G[B], MO: F |>=| G): EnumeratorT[X, B, F] = {
    import MO._
    flatMap(e => EnumeratorT.enumeratorTMonadTrans[X].liftM(MO.promote(ev(e))))
  }

  def bindM[B, G[_]](f: E => G[EnumeratorT[X, B, F]])(implicit F: Monad[F], G: Monad[G]): F[G[EnumeratorT[X, B, F]]] = {
    import scalaz.syntax.semigroup._
    val iter = fold[X, G[EnumeratorT[X, B, F]], F, G[EnumeratorT[X, B, F]]](G.point(EnumeratorT.empty[X, B, F])) {
      case (acc, concat) => G.bind(acc) { en => 
                              G.map(concat) { append => en |+| append } 
                            }
    }   

    (iter &= self.map(f)).run(x => F.point(G.point(pointErr[X, B, F](x))))
  }

  def collect[B](pf: PartialFunction[E, B])(implicit monad: Monad[F]): EnumeratorT[X, B, F] = 
    EnumerateeT.collect[X, E, B, F](pf) run self

  def uniq(implicit ord: Order[E], M: Monad[F]): EnumeratorT[X, E, F] = 
    EnumerateeT.uniq[X, E, F] run self

  def zipWithIndex(implicit M: Monad[F]): EnumeratorT[X, (E, Long), F] = 
    EnumerateeT.zipWithIndex[X, E, F] run self

  def drainTo[M[_]](implicit M: Monad[F], P: PlusEmpty[M], Z: Pointed[M]): F[M[E]] =
    (IterateeT.consume[X, E, F, M] &= self).run(_ => M.point(P.empty)) 

  def reduced[B](b: B)(f: (B, E) => B)(implicit M: Monad[F]): EnumeratorT[X, B, F] = 
    new EnumeratorT[X, B, F] {
      def apply[A] = (step: StepT[X, B, F, A]) => {
        def check(s: StepT[X, E, F, B]): IterateeT[X, B, F, A] = s.fold(
          cont = k => k(eofInput) >>== {
            s => s.mapContOr(_ => sys.error("diverging iteratee"), check(s))
          }
          , done = (a, _) => step.mapCont(f => f(elInput(a)))
          , err  = x => err(x)
        )

        iterateeT(M.bind((IterateeT.fold[X, E, F, B](b)(f) &= self).value) { s => check(s).value })
      }
    }
    
  def cross[E2](e2: EnumeratorT[X, E2, F])(implicit M: Monad[F]): EnumeratorT[X, (E, E2), F] =
    EnumerateeT.cross[X, E, E2, F](e2) run self
}

trait EnumeratorTInstances0 {
  implicit def enumeratorTSemigroup[X, E, F[_]](implicit F0: Bind[F]): Semigroup[EnumeratorT[X, E, F]] = new EnumeratorTSemigroup[X, E, F] {
    implicit def F = F0
  }
}

trait EnumeratorTInstances extends EnumeratorTInstances0 {
  implicit def enumeratorTMonoid[X, E, F[_]](implicit F0: Monad[F]): Monoid[EnumeratorT[X, E, F]] = new EnumeratorTMonoid[X, E, F] {
    implicit def F = F0
  }

  implicit def enumeratorTMonad[X, F[_]](implicit M0: Monad[F]): Monad[({type λ[α]=EnumeratorT[X, α, F]})#λ] = new EnumeratorTMonad[X, F] {
    implicit def M = M0
  }

  implicit def enumeratorTMonadTrans[X]: MonadTrans[({ type λ[β[_], α] = EnumeratorT[X, α, β] })#λ] = new MonadTrans[({ type λ[β[_], α] = EnumeratorT[X, α, β] })#λ] {
    def liftM[G[_]: Monad, E](ga: G[E]): EnumeratorT[X, E, G] = new EnumeratorT[X, E, G] {
      def apply[A] = (s: StepT[X, E, G, A]) => iterateeT(Monad[G].bind(ga) { e => s.mapCont(k => k(elInput(e))).value })
    }

    implicit def apply[G[_]: Monad]: Monad[({type λ[α] = EnumeratorT[X, α, G]})#λ] = enumeratorTMonad[X, G]
  }
}

trait EnumeratorTFunctions {
  def enumerate[E](as: Stream[E]): Enumerator[Unit, E] = enumStream[Unit, E, Id](as)

  def empty[X, E, F[_]: Pointed]: EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A] = _.pointI
    }

  def pointErr[X, E, F[_]: Pointed](x: X): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A] = _ => err[X, E, F, A](x)
    }

  /** 
   * An EnumeratorT that is at EOF
   */
  def enumEofT[X, E, F[_] : Pointed]: EnumeratorT[X, E, F] =
    new EnumeratorT[X, E, F] { 
      def apply[A] = _.mapCont(_(eofInput))
    }

  /**
   * An enumerator that forces the evaulation of an effect in the F monad when it is consumed.
   */
  def perform[X, E, F[_]: Monad, B](f: F[B]): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A] = s => iterateeT(Monad[F].bind(f) { _ => s.pointI.value })
    }

  def enumOne[X, E, F[_]: Pointed](e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      def apply[A] = _.mapCont(_(elInput(e)))
    }

  def enumStream[X, E, F[_] : Monad](xs: Stream[E]): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      def apply[A] = (s: StepT[X, E, F, A]) => xs match {
        case h #:: t => s.mapCont(k => k(elInput(h)) >>== enumStream(t).apply[A])
        case _       => s.pointI
      }
    }

  def enumIterator[X, E, F[_]](x: => Iterator[E])(implicit MO: MonadPartialOrder[F, IO]) : EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      import MO._
      lazy val iter = x
      def apply[A] = (s: StepT[X, E, F, A]) => 
        s.mapCont(
          k =>
            if (iter.hasNext) {
              val n = iter.next()
              k(elInput(n)) >>== apply[A]
            } else s.pointI
        )
    }

  def enumReader[X, F[_]](r: => java.io.Reader)(implicit MO: MonadPartialOrder[F, IO]): EnumeratorT[X, IoExceptionOr[Char], F] = 
    new EnumeratorT[X, IoExceptionOr[Char], F] { 
      import MO._
      lazy val reader = r
      def apply[A] = (s: StepT[X, IoExceptionOr[Char], F, A]) => 
        s.mapCont(
          k => {
            val i = IoExceptionOr(reader.read)
            if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== apply[A]
            else s.pointI
          }
        )
    }

  
  def enumBufferedReaderLines[F[_]](r: => java.io.BufferedReader)(implicit MIO: MonadIO[F]): EnumeratorT[Throwable, String, F] = 
    new EnumeratorT[Throwable, String, F] {
      lazy val reader = r
      
      def apply[A] = (s: StepT[Throwable, String, F, A]) => 
        s.mapCont(k => tryIO(IO(reader.readLine)) flatMap (l => 
          if (l == null) s.pointI else k(Input(l)) >>== apply[A]))
    }

  /**
   * An enumerator that yields the elements of the specified array from index min (inclusive) to max (exclusive)
   */
  def enumArray[X, E, F[_]: Monad](a : Array[E], min: Int = 0, max: Option[Int] = None) : EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      private val limit = max.map(_ min (a.length)).getOrElse(a.length)
      def apply[A] = {
        def loop(pos : Int): StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
          s => 
            s.mapCont(
              k => if (limit > pos) k(elInput(a(pos))) >>== loop(pos + 1)
                   else             s.pointI
            )   
        }

        loop(min)
      }
    }

  def repeat[X, E, F[_] : Monad](e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A] = (s: StepT[X, E, F, A]) => s.mapCont(_(elInput(e)) >>== apply[A])
    }

  def iterate[X, E, F[_] : Monad](f: E => E, e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A]: StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
        type StepM = StepT[X, E, F, A]
        type IterateeM = IterateeT[X, E, F, A]

        def checkCont1(z: (E => (StepM => IterateeM)) => E => (Input[E] => IterateeM) => IterateeM, lastState: E): (StepM => IterateeM) = {
          def step: E => (StepM => IterateeM) = {
            state => _.mapCont(k => z(step)(state)(k))
          }

          step(lastState)
        }

        checkCont1(contFactory => state => k => k(elInput(e)) >>== contFactory(f(state)), e)
      }
    }
}

// Instances are mixed in with the IterateeT object
object EnumeratorT extends EnumeratorTFunctions with EnumeratorTInstances

//
// Type class implementation traits
//

private[scalaz] trait EnumeratorTSemigroup[X, E, F[_]] extends Semigroup[EnumeratorT[X, E, F]] {
  implicit def F: Bind[F]

  def append(f1: EnumeratorT[X, E, F], f2: => EnumeratorT[X, E, F]) = 
    new EnumeratorT[X, E, F] {
      def apply[A] = (s: StepT[X, E, F, A]) => f1[A](s) >>== f2[A]
    }
}

private[scalaz] trait EnumeratorTMonoid[X, E, F[_]] extends Monoid[EnumeratorT[X, E, F]] with EnumeratorTSemigroup[X, E, F] {
  implicit def F: Monad[F]

  def zero = new EnumeratorT[X, E, F] {
    def apply[A] = (s: StepT[X, E, F, A]) => s.pointI
  }
}

private[scalaz] trait EnumeratorTFunctor[X, F[_]] extends Functor[({type λ[α]=EnumeratorT[X, α, F]})#λ] {
  implicit def M: Monad[F]
  abstract override def map[A, B](fa: EnumeratorT[X, A, F])(f: A => B): EnumeratorT[X, B, F] = fa.map(f)
}

private[scalaz] trait EnumeratorTPointed[X, F[_]] extends Pointed[({type λ[α]=EnumeratorT[X, α, F]})#λ] with EnumeratorTFunctor[X, F] {
  def point[E](e: => E) = EnumeratorT.enumOne[X, E, F](e)
}

private [scalaz] trait EnumeratorTMonad[X, F[_]] extends Monad[({type λ[α]=EnumeratorT[X, α, F]})#λ] with EnumeratorTPointed[X, F] {
  def bind[A, B](fa: EnumeratorT[X, A, F])(f: A => EnumeratorT[X, B, F]) = fa.flatMap(f)
}
