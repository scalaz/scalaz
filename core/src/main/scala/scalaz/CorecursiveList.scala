package scalaz

import scala.annotation.tailrec
import Maybe.{Empty, Just, just}

/** The corecursive list; i.e. the arguments to `unfold` saved off to
  * a data structure.  Generally does not have methods itself; as with
  * [[scalaz.NonEmptyList]], it provides typeclass instances instead,
  * and you typically import typeclass syntax to get methods.
  *
  * The corecursive list can be a very efficient way to represent
  * "listlike" things in some cases; because it is so unlike "normal"
  * collections, though, it's important to understand how its
  * operations are implemented and what their performance
  * characteristics are going to be.  For example, using `cons`
  * iteratively to add a bunch of elements to the beginning of a
  * [[scala.collection.immutable.List]] is very efficient; it's very
  * ''inefficient'' for corecursive list.
  *
  * Operations are generally designed to preserve the isomorphism with
  * [[scalaz.EphemeralStream]]; for example, `ap` could be a "zipping"
  * `ap`, but instead is a less efficient "combination" style.  This
  * means that the [[scalaz.Monad]] has the same behavior as that of
  * [[scalaz.EphemeralStream]] and more traditional strict list
  * structures.
  */
sealed abstract class CorecursiveList[A] {
  type S
  val init: S
  val step: S => Maybe[(S, A)]
}

object CorecursiveList extends CorecursiveListInstances {
  private[this] final case class CorecursiveListImpl[S0, A](
    init: S0, step: S0 => Maybe[(S0, A)]) extends CorecursiveList[A] {
    type S = S0
  }

  def apply[S, A](init: S)(step: S => Maybe[(S, A)]): CorecursiveList[A] =
    CorecursiveListImpl(init, step)

  import scalaz.Isomorphism.{<~>, IsoFunctorTemplate}

  val ephemeralStreamIso: EphemeralStream <~> CorecursiveList =
    new IsoFunctorTemplate[EphemeralStream, CorecursiveList] {
      def to[A](fa: EphemeralStream[A]) =
        CorecursiveList(fa)(fa =>
          if (fa.isEmpty) Empty() else just((fa.tail(), fa.head())))

      def from[A](ga: CorecursiveList[A]) =
        EphemeralStream.unfold(ga.init)(s =>
          ga.step(s).map{case (s, a) => (a, s)}.toOption)
    }

  val streamIso: Stream <~> CorecursiveList =
    new IsoFunctorTemplate[Stream, CorecursiveList] {
      def to[A](fa: Stream[A]) =
        CorecursiveList(fa){
          case Stream.Empty => Empty()
          case x #:: xs => just((xs, x))
        }

      def from[A](ga: CorecursiveList[A]) = {
        def rec(s: ga.S): Stream[A] =
          ga.step(s) cata ({case (s, a) => a #:: rec(s)}, Stream())
        rec(ga.init)
      }
    }

  import scala.collection.immutable.{IndexedSeq, LinearSeq}

  /** Any `LinearSeq` converts to a `CorecursiveList` efficiently.  No
    * natural transformation from `CorecursiveList` to `List` exists,
    * because representation of infinite lists is not guaranteed.  Use
    * `streamIso` for such cases instead.
    */
  val fromList: LinearSeq ~> CorecursiveList =
    new (LinearSeq ~> CorecursiveList) {
      def apply[A](fa: LinearSeq[A]) =
        CorecursiveList(fa){
          case a +: fa => just((fa, a))
          case _ => Empty()
        }
    }

  /** Any `IndexedSeq` converts to a `CorecursiveList` efficiently.  No
    * natural transformation from `CorecursiveList` to `Vector`
    * exists, because representation of infinite lists is not
    * guaranteed.  Use `streamIso` for such cases instead.
    */
  val fromVector: IndexedSeq ~> CorecursiveList =
    new (IndexedSeq ~> CorecursiveList) {
      def apply[A](fa: IndexedSeq[A]) =
        CorecursiveList(0){ix =>
          if (ix < fa.length) just((ix + 1, fa(ix)))
          else Empty()
        }
    }

  def fromStream[A](s: Stream[A]): CorecursiveList[A] =
    streamIso.to(s)

  /** A frightfully inefficient way to add elements to the beginning of
    * a corecursive list.  It is correct, and reasonable enough for
    * one-or-two-time use on a particular list, but be warned that
    * iterative cons is necessarily a very inefficient way to build up
    * a `CorecursiveList`.
    *
    * You can see why by taking a look at its choice of state.  `cons`
    * adds a `Maybe` to the state of the tail list, so if you cons
    * four elements onto `someCList`, its state will be
    * `Maybe[Maybe[Maybe[Maybe[someCList.S]]]]`, and so on.  This is
    * utterly unsustainable for iterative consing.
    * 
    * Instead, try `apply`, or another more wholemeal-style
    * combinator.  You might also cons up a different structure, then
    * transform that to a `CorecursiveList` wholesale and `append` or
    * `plus` it on; that's reasonably efficient, too.
    */
  def cons[A](a: A, fa: CorecursiveList[A]): CorecursiveList[A] =
    CorecursiveList(Empty(): Maybe[fa.S]){ms =>
      ms cata (s => fa.step(s) map {case (fas, a) => (just(fas), a)},
               just((just(fa.init), a)))
    }

  implicit val covariantInstance:
      MonadPlus[CorecursiveList] with Foldable[CorecursiveList]
      with IsEmpty[CorecursiveList] with Align[CorecursiveList]
      with Zip[CorecursiveList] =
    new MonadPlus[CorecursiveList] with Foldable.FromFoldr[CorecursiveList]
        with IsEmpty[CorecursiveList] with Align[CorecursiveList]
        with Zip[CorecursiveList] {
      override def map[A, B](fa: CorecursiveList[A])(f: A => B) =
        CorecursiveList(fa.init)(fa.step andThen (_ map {
          case (s, a) => (s, f(a))
        }))

      override def ap[A, B](fa0: => CorecursiveList[A])(ff0: => CorecursiveList[A => B]) = {
        val fa = fa0
        val ff = ff0
        def bstep(sa: fa.S, ffs: Maybe[(ff.S, A => B)])
            : Maybe[((fa.S, Maybe[(ff.S, A => B)]), B)] = ffs match {
          case Empty() => ff.step(ff.init) flatMap (st => bstep(sa, just(st)))
          case Just((sf, f)) =>
            (fa.step(sa) map {case (sa, a) => ((sa, ffs), f(a))}
              orElse ff.step(sf).flatMap(st => bstep(fa.init, just(st))))
        }
        CorecursiveList((fa.init, Empty(): Maybe[(ff.S, A => B)])
                      )((bstep _).tupled)
      }

      override def bind[A, B](fa: CorecursiveList[A])(f: A => CorecursiveList[B]) = {
        def bstep(sa: fa.S, mfb: Maybe[CorecursiveList[B]])
            : Maybe[((fa.S, Maybe[CorecursiveList[B]]), B)] = mfb match {
          case Empty() => fa.step(sa) flatMap {case (sa, a) =>
            bstep(sa, just(f(a)))
          }
          case Just(fb) => fb.step(fb.init) map {
            case (sb, b) => ((sa, just(CorecursiveList(sb)(fb.step))), b)
          } orElse bstep(sa, Empty())
        }
        CorecursiveList((fa.init, Empty(): Maybe[CorecursiveList[B]])){
          case (sa, mfb) => bstep(sa, mfb)
        }
      }

      override def filter[A](fa: CorecursiveList[A])(f: A => Boolean) = {
        @tailrec def step(s: fa.S): Maybe[(fa.S, A)] = fa.step(s) match {
          case Empty() => Empty()
          case j@Just((s, a)) =>
            if (f(a)) j else step(s)
        }
        CorecursiveList(fa.init)(step(_))
      }

      override def point[A](a: => A) =
        CorecursiveList(false){b =>
          if (b) Empty() else just((true, a))
        }

      override def foldRight[A, B](fa: CorecursiveList[A], z: => B)(f: (A, => B) => B) = {
        def rec(s: fa.S): B =
          fa.step(s) cata ({case (s, a) => f(a, rec(s))}, z)
        rec(fa.init)
      }

      override def foldLeft[A, B](fa: CorecursiveList[A], z: B)(f: (B, A) => B) = {
        @tailrec def rec(z: B, s: fa.S): B = fa.step(s) match {
          case Empty() => z
          case Just((s, a)) => rec(f(z, a), s)
        }
        rec(z, fa.init)
      }

      override def empty[A] =
        CorecursiveList(())(Function const Empty())

      override def plus[A](la: CorecursiveList[A], ra0: => CorecursiveList[A]) = {
        lazy val ra = ra0
        type SS = la.S \/ ra.S
        def rightStep(rs: ra.S) =
          ra.step(rs) map {case (rs, a) => (\/-(rs): SS, a)}
        CorecursiveList(-\/(la.init): SS){
          case -\/(ls) =>
            (la.step(ls) map {case (ls, a) => (-\/(ls): SS, a)}
              orElse rightStep(ra.init))
          case \/-(rs) =>
            rightStep(rs)
        }
      }

      override def isEmpty[A](fa: CorecursiveList[A]) =
        fa.step(fa.init).isEmpty

      override def alignWith[A, B, C](f: A \&/ B => C) = (fa, fb) => {
        import \&/.{Both, This, That}
        CorecursiveList(Both(fa.init, fb.init): fa.S \&/ fb.S){
          case Both(sa, sb) =>
            Align[Maybe].align(fa.step(sa), fb.step(sb)).map(sasb =>
              (sasb bimap (_._1, _._1), f(sasb bimap (_._2, _._2))))
          case This(sa) =>
            fa.step(sa) map {case (sa, a) => (This(sa), f(This(a)))}
          case That(sb) =>
            fb.step(sb) map {case (sb, b) => (That(sb), f(That(b)))}
        }
      }

      override def zip[A, B](l: => CorecursiveList[A], r: => CorecursiveList[B]) = {
        val l1 = l
        val r1 = r
        CorecursiveList((l1.init, r1.init)){
          case (ls, rs) =>
            Apply[Maybe].apply2(l1.step(ls), r1.step(rs)){
              case ((ls, la), (rs, ra)) => ((ls, rs), (la, ra))
            }
        }
      }
    }

  implicit def monoidInstance[A]: Monoid[CorecursiveList[A]] =
    covariantInstance.monoid

  implicit def orderInstance[A: Order]: Order[CorecursiveList[A]] =
    new Order[CorecursiveList[A]] with CorecursiveListEqual[A] {
      override val A: Order[A] = Order[A]
      def order(l: CorecursiveList[A], r: CorecursiveList[A]) = {
        @tailrec def rec(ls: l.S, rs: r.S): Ordering =
          (l.step(ls), r.step(rs)) match {
            case (Empty(), right) => right match {
              case Empty() => Ordering.EQ
              case _ => Ordering.LT
            }
            case (Just((ls2, la)), Just((rs2, ra))) =>
              A.order(la, ra) match {
                case Ordering.EQ => rec(ls2, rs2)
                case o => o
              }
            case _ => Ordering.GT
          }
        rec(l.init, r.init)
      }
    }
}

sealed abstract class CorecursiveListInstances {
  implicit def equalInstance[A: Equal]: Equal[CorecursiveList[A]] =
    new CorecursiveListEqual[A] {
      val A = Equal[A]
    }
}

private sealed trait CorecursiveListEqual[A] extends Equal[CorecursiveList[A]] {
  val A: Equal[A]

  override def equal(l: CorecursiveList[A], r: CorecursiveList[A]): Boolean = {
    @tailrec def rec(ls: l.S, rs: r.S): Boolean =
      (l.step(ls), r.step(rs)) match {
        case (Empty(), Empty()) => true
        case (Just((ls2, la)), Just((rs2, ra))) =>
          A.equal(la, ra) && rec(ls2, rs2)
        case _ => false
      }
    rec(l.init, r.init)
  }
}
