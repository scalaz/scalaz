package scalaz
package iteratee

import effect._
import Iteratee._
import Id._

/**
 * A data sink.
 *
 * Represents a value of type `F[StepT[E, F, A]]`
 *
 * @see [[scalaz.iteratee.StepT]]
 *
 * @tparam E The type of the input data (mnemonic: '''E'''lement type)
 * @tparam F The type constructor representing an effect.
 *           The type constructor [[scalaz.Id]] is used to model pure computations, and is fixed as such in the type alias [[scalaz.iteratee.Iteratee]].
 * @tparam A The type of the calculated result
 */
sealed abstract class IterateeT[E, F[_], A] {
  def value: F[StepT[E, F, A]]

  def foldT[Z](
                cont: (Input[E] => IterateeT[E, F, A]) => F[Z]
                , done: (=> A, => Input[E]) => F[Z]
                )(implicit F: Bind[F]): F[Z] =
    F.bind(value)((s: StepT[E, F, A]) => s(cont, done))

  /**
   * Run this iteratee
   */
  def run(implicit F: Monad[F]): F[A] = {
    F.bind((this &= enumEofT[E, F]).value)((s: StepT[E, F, A]) => s.fold(
      cont = _ => sys.error("diverging iteratee")
      , done = (a, _) => F.point(a)
    ))
  }

  def flatMap[B](f: A => IterateeT[E, F, B])(implicit F: Monad[F]): IterateeT[E, F, B] = {
    def through(x: IterateeT[E, F, A]): IterateeT[E, F, B] =
      iterateeT(
        F.bind(x.value)((s: StepT[E, F, A]) => s.fold[F[StepT[E, F, B]]](
          cont = k => F.point(StepT.scont(u => through(k(u))))
          , done = (a, i) =>
            if (i.isEmpty)
              f(a).value
            else
              F.bind(f(a).value)(_.fold(
                cont = kk => kk(i).value
                , done = (aa, _) => F.point(StepT.sdone[E, F, B](aa, i))
              ))
        )))
    through(this)
  }

  def map[B](f: A => B)(implicit F: Monad[F]): IterateeT[E, F, B] = {
    flatMap(a => StepT.sdone[E, F, B](f(a), emptyInput).pointI)
  }

  def contramap[EE](f: EE => E)(implicit F: Monad[F]): IterateeT[EE, F, A] = {
    def step(s: StepT[E, F, A]): IterateeT[EE, F, A] = s.fold[IterateeT[EE, F, A]](
      cont = k => cont((in: Input[EE]) => k(in.map(i => f(i))) >>== step),
      done = (a, i) => done(a, if (i.isEof) eofInput else emptyInput)
    )

    this >>== step
  }

  /**
   * A generalization of >>== that allows a step function which returns its result in a different, "bigger" monad.
   * The monad for G must perform all the effects of F as part of its evaluation; in the trivial case, of course
   * F and G will have the same monad.
   */
  def advance[EE, AA, G[_]](f: StepT[E, F, A] => IterateeT[EE, G, AA])(implicit MO: G |>=| F): IterateeT[EE, G, AA] = {
    iterateeT(MO.MG.bind(MO.promote(value))(s => f(s).value))
  }

  def advanceT[EE, AA, G[_]](f: StepT[E, F, A] => G[StepT[EE, F, AA]])(implicit MO: G |>=| F): G[StepT[EE, F, AA]] = {
    MO.MG.bind(MO.promote(value))(s => f(s))
  }

  /**
   * Combine this Iteratee with an Enumerator-like function.
   *
   * Often used in combination with the implicit views such as `enumStream` and `enumIterator`, for example:
   *
   * {{{
   * head[Unit, Int, Id] >>== Stream.continually(1) // enumStream(Stream.continually(1))
   * }}}
   *
   * @param f An Enumerator-like function. If the type parameters `EE` and `BB` are chosen to be
   *          `E` and `B` respectively, the type of `f` is equivalent to `EnumeratorT[E, F, A]`.
   */
  def >>==[EE, AA](f: StepT[E, F, A] => IterateeT[EE, F, AA])(implicit F: Bind[F]): IterateeT[EE, F, AA] =
    iterateeT(F.bind(value)(s => f(s).value))

  def %=[O](e: EnumerateeT[O, E, F])(implicit m: Monad[F]): IterateeT[O, F, A] =
    (this >>== e[A]).joinI[E, A]

  def &=(e: EnumeratorT[E, F])(implicit F: Bind[F]): IterateeT[E, F, A] =
    this >>== e[A]

  def mapI[G[_]](f: F ~> G)(implicit F: Functor[F]): IterateeT[E, G, A] = {
    def step: StepT[E, F, A] => StepT[E, G, A] =
      _.fold(
        cont = k => scont[E, G, A](k andThen loop)
        , done = (a, i) => sdone[E, G, A](a, i)
      )
    def loop: IterateeT[E, F, A] => IterateeT[E, G, A] = i => iterateeT(f(F.map(i.value)(step)))
    loop(this)
  }

  def up[G[_]](implicit G: Applicative[G], F: Comonad[F]): IterateeT[E, G, A] = {
    mapI(new (F ~> G) {
      def apply[A](a: F[A]) = G.point(F.copoint(a))
    })
  }

  def joinI[I, B](implicit outer: IterateeT[E, F, A] =:= IterateeT[E, F, StepT[I, F, B]], M: Monad[F]): IterateeT[E, F, B] = {
    val M0 = IterateeT.IterateeTMonad[E, F]
    def check: StepT[I, F, B] => IterateeT[E, F, B] = _.fold(
      cont = k => k(eofInput) >>== {
        s => s.mapContOr(_ => sys.error("diverging iteratee"), check(s))
      }
      , done = (a, _) => M0.point(a)
    )

    outer(this) flatMap check
  }

  /**
   * Feeds input elements to this iteratee until it is done, feeds the produced value to the
   * inner iteratee.  Then this iteratee will start over, looping until the inner iteratee is done.
   */
  def sequenceI(implicit m: Monad[F]): EnumerateeT[E, A, F] =
    new EnumerateeT[E, A, F] {
      def apply[B] = {
        def loop = doneOr(checkEof)
        def checkEof: (Input[A] => IterateeT[A, F, B]) => IterateeT[E, F, StepT[A, F, B]] = k =>
          isEof[E, F] flatMap {
            eof =>
              if (eof) done(scont(k), eofInput)
              else step(k)
          }
        def step: (Input[A] => IterateeT[A, F, B]) => IterateeT[E, F, StepT[A, F, B]] = k =>
          flatMap (a => k(elInput(a)) >>== loop)
        loop
      }
    }

  def zip[B](other: IterateeT[E, F, B])(implicit F: Monad[F]): IterateeT[E, F, (A, B)] = {
    def step[Z](i: IterateeT[E, F, Z], in: Input[E]) =
      IterateeT.IterateeTMonadTrans[E].liftM(i.foldT[(Option[(Z, Input[E])], IterateeT[E, F, Z])](
        cont = k => F.point((None, k(in)))
        , done = (a, x) => F.point((Some((a, x)), done(a, x)))
      ))
    def loop(x: IterateeT[E, F, A], y: IterateeT[E, F, B])(in: Input[E]): IterateeT[E, F, (A, B)] = in(
      el = _ =>
        step(x, in) flatMap {
          case (a, xx) =>
            step(y, in) flatMap {
              case (b, yy) => (a, b) match {
                case (Some((a, e)), Some((b, ee))) => done((a, b), if (e.isEl) e else ee)
                case _                             => cont(loop(xx, yy))
              }
            }
        }
      , empty = cont(loop(x, y))
      , eof = (x &= enumEofT[E, F]) flatMap (a => (y &= enumEofT[E, F]) map (b => (a, b)))
    )
    cont(loop(this, other))
  }
}

object IterateeT extends IterateeTInstances with IterateeTFunctions {
  def apply[E, F[_], A](s: F[StepT[E, F, A]]): IterateeT[E, F, A] =
    iterateeT(s)
}

sealed abstract class IterateeTInstances0 {
  implicit def IterateeTMonad[E, F[_]](implicit F0: Monad[F]): Monad[IterateeT[E, F, ?]] =
    new IterateeTMonad[E, F] {
      implicit def F = F0
    }

  implicit def IterateeMonad[E]: Monad[Iteratee[E, ?]] = IterateeTMonad[E, Id]

  implicit def IterateeTMonadTransT[E, H[_[_], _]](implicit T0: MonadTrans[H]): MonadTrans[λ[(α[_], β) => IterateeT[E, H[α, ?], β]]] =
    new IterateeTMonadTransT[E, H] {
      implicit def T = T0
    }
}

sealed abstract class IterateeTInstances extends IterateeTInstances0 {
  implicit def IterateeTMonadTrans[E]: Hoist[λ[(α[_], β) => IterateeT[E, α, β]]] =
    new IterateeTHoist[E] { }

  implicit def IterateeTHoistT[E, H[_[_], _]](implicit T0: Hoist[H]): Hoist[λ[(α[_], β) => IterateeT[E, H[α, ?], β]]] =
    new IterateeTHoistT[E, H] {
      implicit def T = T0
    }

  implicit def IterateeTMonadIO[E, F[_]](implicit M0: MonadIO[F]): MonadIO[IterateeT[E, F, ?]] =
    new IterateeTMonadIO[E, F] {
      implicit def F = M0
    }

  implicit def IterateeTContravariant[F[_]: Monad, A]: Contravariant[IterateeT[?, F, A]] =
    new Contravariant[IterateeT[?, F, A]] {
      def contramap[E, EE](r: IterateeT[E, F, A])(f: EE => E) = r.contramap(f)
    }
}

trait IterateeTFunctions {
  def iterateeT[E, F[_], A](s: F[StepT[E, F, A]]): IterateeT[E, F, A] = new IterateeT[E, F, A] {
    val value = s
  }

  def cont[E, F[_] : Applicative, A](c: Input[E] => IterateeT[E, F, A]): IterateeT[E, F, A] =
    StepT.scont(c).pointI

  def done[E, F[_] : Applicative, A](d: => A, r: => Input[E]): IterateeT[E, F, A] =
    StepT.sdone(d, r).pointI

  /**
   * An iteratee that writes input to the output stream as it comes in.  Useful for debugging.
   */
  def putStrTo[E](os: java.io.OutputStream)(implicit s: Show[E]): IterateeT[E, IO, Unit] = {
    def write(e: E) = IO(os.write(s.shows(e).getBytes))
    foldM(())((_: Unit, e: E) => write(e))
  }

  /**
   * An iteratee that consumes all of the input into something that is PlusEmpty and Applicative.
   */
  def consume[E, F[_]: Monad, A[_]: PlusEmpty : Applicative]: IterateeT[E, F, A[E]] = {
    import scalaz.syntax.plus._
    def step(e: Input[E]): IterateeT[E, F, A[E]] =
      e.fold(empty = cont(step)
        , el = e => cont(step).map(a => Applicative[A].point(e) <+> a)
        , eof = done(PlusEmpty[A].empty, eofInput[E])
      )

    cont(step)
  }

  def collectT[E, F[_], A[_]](implicit M: Monad[F], mae: Monoid[A[E]], pointed: Applicative[A]): IterateeT[E, F, A[E]] = {
    import scalaz.syntax.semigroup._
    def step(e: Input[E]): IterateeT[E, F, A[E]] =
      e.fold(empty = cont(step)
        , el = e => cont(step).map(a => Applicative[A].point(e) |+| a)
        , eof = done(Monoid[A[E]].zero, eofInput[E])
      )

    cont(step)
  }

  /**An iteratee that consumes the head of the input **/
  def head[E, F[_] : Applicative]: IterateeT[E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[E, F, Option[E]] =
      s(empty = cont(step)
        , el = e => done(Some(e), emptyInput[E])
        , eof = done(None, eofInput[E])
      )
    cont(step)
  }

  def headDoneOr[E, F[_] : Monad, B](b: => B, f: E => IterateeT[E, F, B]): IterateeT[E, F, B] = {
    head[E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that returns the first element of the input **/
  def peek[E, F[_] : Applicative]: IterateeT[E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[E, F, Option[E]]
    = s(el = e => done(Some(e), s),
      empty = cont(step),
      eof = done(None, eofInput[E]))
    cont(step)
  }

  def peekDoneOr[E, F[_] : Monad, B](b: => B, f: E => IterateeT[E, F, B]): IterateeT[E, F, B] = {
    peek[E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that skips the first n elements of the input **/
  def drop[E, F[_] : Applicative](n: Int): IterateeT[E, F, Unit] = {
    def step(s: Input[E]): IterateeT[E, F, Unit] =
      s(el = _ => drop(n - 1),
        empty = cont(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else cont(step)
  }

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  def dropWhile[E, F[_] : Applicative](p: E => Boolean): IterateeT[E, F, Unit] = {
    def step(s: Input[E]): IterateeT[E, F, Unit] =
      s(el = e => if (p(e)) dropWhile(p) else done((), s),
        empty = cont(step),
        eof = done((), eofInput[E]))
    cont(step)
  }

  /**
   * An iteratee that skips elements until the predicate evaluates to true.
   */
  def dropUntil[E, F[_] : Applicative](p: E => Boolean): IterateeT[E, F, Unit] = dropWhile(!p(_))

  def fold[E, F[_] : Applicative, A](init: A)(f: (A, E) => A): IterateeT[E, F, A] = {
    def step(acc: A): Input[E] => IterateeT[E, F, A] = s =>
      s(el = e => cont(step(f(acc, e))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(init))
  }

  def foldM[E, F[_], A](init: A)(f: (A, E) => F[A])(implicit m: Monad[F]): IterateeT[E, F, A] = {
    def step(acc: A): Input[E] => IterateeT[E, F, A] = s =>
      s(el = e => IterateeT.IterateeTMonadTrans[E].liftM(f(acc, e)) flatMap (a => cont(step(a))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(init))
  }

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  def length[E, F[_] : Applicative]: IterateeT[E, F, Int] = fold(0)((a, _) => a + 1)

  /**
   * An iteratee that checks if the input is EOF.
   */
  def isEof[E, F[_] : Applicative]: IterateeT[E, F, Boolean] = cont(in => done(in.isEof, in))

  def sum[E: Monoid, F[_]: Monad]: IterateeT[E, F, E] =
    foldM[E, F, E](Monoid[E].zero)((a, e) => Applicative[F].point(Monoid[E].append(a, e)))
}

//
// Type class implementation traits
//

private trait IterateeTMonad[E, F[_]] extends Monad[IterateeT[E, F, ?]] {
  implicit def F: Monad[F]

  def point[A](a: => A) = StepT.sdone(a, emptyInput).pointI
  override def map[A, B](fa: IterateeT[E, F, A])(f: A => B): IterateeT[E, F, B] = fa map f
  def bind[A, B](fa: IterateeT[E, F, A])(f: A => IterateeT[E, F, B]): IterateeT[E, F, B] = fa flatMap f
}

private trait IterateeTHoist[E] extends Hoist[λ[(β[_], α) => IterateeT[E, β, α]]] {
  trait IterateeTF[F[_]] {
    type λ[α] = IterateeT[E, F, α]
  }

  def hoist[F[_]: Monad, G[_]](f: F ~> G) = new (IterateeTF[F]#λ ~> IterateeTF[G]#λ) {
    def apply[A](fa: IterateeT[E, F, A]): IterateeT[E, G, A] = fa mapI f
  }

  def liftM[G[_] : Monad, A](ga: G[A]): IterateeT[E, G, A] =
    iterateeT(Monad[G].map(ga)(sdone[E, G, A](_, emptyInput)))

  implicit def apply[G[_] : Monad]: Monad[IterateeTF[G]#λ] = IterateeT.IterateeTMonad[E, G]
}

private trait IterateeTMonadIO[E, F[_]] extends MonadIO[IterateeT[E, F, ?]] with IterateeTMonad[E, F] {
  implicit def F: MonadIO[F]

  def liftIO[A](ioa: IO[A]) = MonadTrans[λ[(α[_], β) => IterateeT[E, α, β]]].liftM(F.liftIO(ioa))
}

private trait IterateeTMonadTransT[E, H[_[_], _]] extends MonadTrans[λ[(α[_], β) => IterateeT[E, H[α, ?], β]]] {
  implicit def T: MonadTrans[H]

  def liftM[G[_]: Monad, A](ga: G[A]): IterateeT[E, H[G, ?], A] =
    IterateeT.IterateeTMonadTrans[E].liftM[H[G, ?], A](T.liftM(ga))(T[G])

  def apply[G[_]: Monad]: Monad[IterateeT[E, H[G, ?], ?]] =
    IterateeT.IterateeTMonad[E, H[G, ?]](T[G])
}

private trait IterateeTHoistT[E, H[_[_], _]] extends Hoist[λ[(α[_], β) => IterateeT[E, H[α, ?], β]]] with IterateeTMonadTransT[E, H] {
  implicit def T: Hoist[H]

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (IterateeT[E, H[M, ?], ?] ~> IterateeT[E, H[N, ?], ?]) {
    def apply[A](fa: IterateeT[E, H[M, ?], A]): IterateeT[E, H[N, ?], A] =
      fa.mapI[H[N, ?]](T.hoist[M, N](f))(T[M])
  }
}
