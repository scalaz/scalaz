package scalaz

/** @since 7.0.3 */
sealed abstract class \&/[+A, +B] extends Product with Serializable {
  import \&/._

  def isThis: Boolean =
    this match {
      case This(_) => true
      case That(_) => false
      case Both(_, _) => false
    }

  def isThat: Boolean =
    this match {
      case This(_) => false
      case That(_) => true
      case Both(_, _) => false
    }

  def isBoth: Boolean =
    this match {
      case This(_) => false
      case That(_) => false
      case Both(_, _) => true
    }

  def a: Option[A] =
    this match {
      case This(a) => Some(a)
      case That(_) => None
      case Both(a, _) => Some(a)
    }

  def b: Option[B] =
    this match {
      case This(_) => None
      case That(b) => Some(b)
      case Both(_, b) => Some(b)
    }

  def onlyThis: Option[A] =
    this match {
      case This(a) => Some(a)
      case That(_) => None
      case Both(_, _) => None
    }

  def onlyThat: Option[B] =
    this match {
      case This(_) => None
      case That(b) => Some(b)
      case Both(_, _) => None
    }

  def onlyThisOrThat: Option[A \/ B] =
    this match {
      case This(a) => Some(-\/(a))
      case That(b) => Some(\/-(b))
      case Both(_, _) => None
    }

  def onlyBoth: Option[(A, B)] =
    this match {
      case This(_) => None
      case That(_) => None
      case Both(a, b) => Some(a, b)
    }

  def pad: (Option[A], Option[B]) =
    this match {
      case This(a) => (Some(a), None)
      case That(b) => (None, Some(b))
      case Both(a, b) => (Some(a), Some(b))
    }

  def fold[X](s: A => X, t: B => X, q: (A, B) => X): X =
    this match {
      case This(a) => s(a)
      case That(b) => t(b)
      case Both(a, b) => q(a, b)
    }

  def swap: (B \&/ A) =
    this match {
      case This(a) => That(a)
      case That(b) => This(b)
      case Both(a, b) => Both(b, a)
    }

  def unary_~ : (B \&/ A) =
    swap

  def swapped[AA, BB](k: (B \&/ A) => (BB \&/ AA)): (AA \&/ BB) =
    k(swap).swap

  def ~[AA, BB](k: (B \&/ A) => (BB \&/ AA)): (AA \&/ BB) =
    swapped(k)

  def append[AA >: A, BB >: B](that: => (AA \&/ BB))(implicit SA: Semigroup[AA], SB: Semigroup[BB]): (AA \&/ BB) =
    (this, that) match {
      case (This(a1),     This(a2))     => This(SA.append(a1, a2))
      case (This(a1),     Both(a2, b))  => Both(SA.append(a1, a2), b)
      case (This(a),      That(b))      => Both(a,                 b)
      case (Both(a1, b),  This(a2))     => Both(SA.append(a1, a2), b)
      case (Both(a1, b1), Both(a2, b2)) =>
        Both(SA.append(a1, a2), SB.append(b1, b2))
      case (Both(a, b1),  That(b2))     => Both(a, SB.append(b1, b2))
      case (That(b),      This(a))      => Both(a, b)
      case (That(b1),     Both(a, b2))  => Both(a, SB.append(b1, b2))
      case (That(b1),     That(b2))     => That(   SB.append(b1, b2))
    }

  def bimap[C, D](f: A => C, g: B => D): (C \&/ D) =
    this match {
      case This(a) => This(f(a))
      case That(b) => That(g(b))
      case Both(a, b) => Both(f(a), g(b))
    }

  def leftMap[C](f: A => C): (C \&/ B) =
    bimap(f, identity)

  def bitraverse[F[_]: Apply, C, D](f: A => F[C], g: B => F[D]): F[C \&/ D] =
    this match {
      case This(a) =>
        Functor[F].map(f(a))(This(_))
      case That(b) =>
        Functor[F].map(g(b))(That(_))
      case Both(a, b) =>
        Apply[F].apply2(f(a), g(b)) {
          case (c, d) => Both(c, d): C \&/ D
        }
    }

  def map[D](g: B => D): (A \&/ D) =
    bimap(identity, g)

  def traverse[F[_]: Applicative, AA >: A, D](g: B => F[D]): F[AA \&/ D] =
    this match {
      case a @ This(_) =>
        Applicative[F].point(a)
      case That(b) =>
        Functor[F].map(g(b))(That(_))
      case Both(a, b) =>
        Functor[F].map(g(b))(Both(a, _): A \&/ D)
    }

  def foreach(g: B => Unit): Unit =
    bimap(_ => (), g)

  def flatMap[AA >: A, D](g: B => (AA \&/ D))(implicit M: Semigroup[AA]): (AA \&/ D) =
    this match {
      case a @ This(_) =>
        a
      case That(b) =>
        g(b)
      case Both(a, b) =>
        g(b) match {
          case This(aa) =>
            This(M.append(a, aa))
          case That(bb) =>
            Both(a, bb)
          case Both(aa, bb) =>
            Both(M.append(a, aa), bb)
        }
    }

  def &&&[AA >: A, C](t: AA \&/ C)(implicit M: Semigroup[AA]): AA \&/ (B, C) =
    for {
      b <- this
      c <- t
    } yield (b, c)

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z): Z =
    b match {
      case None => z
      case Some(bb) => f(bb, z)
    }

  def foldMap[C](f: B => C)(implicit M: Monoid[C]): C =
    b match {
      case None => M.zero
      case Some(bb) => f(bb)
    }

  def bifoldRight[Z](z: => Z)(f: (A, => Z) => Z)(g: (B, => Z) => Z): Z =
    this match{
      case This(a)    => f(a, z)
      case That(b)    => g(b, z)
      case Both(a, b) => f(a, g(b, z))
    }

  def bifoldMap[M](f: A => M)(g: B => M)(implicit M: Semigroup[M]): M =
    fold(f, g, (a, b) => M.append(f(a), g(b)))

  def exists(p: B => Boolean): Boolean =
    b exists p

  def forall(p: B => Boolean): Boolean =
    b forall p

  def toList: List[B] =
    b.toList

  def getOrElse[BB >: B](bb: => BB): BB =
    b getOrElse bb

  def |[BB >: B](bb: => BB): BB =
    getOrElse(bb)

  def valueOr[BB >: B](x: A => BB)(implicit M: Semigroup[BB]): BB =
    this match {
      case This(a) =>
        x(a)
      case That(b) =>
        b
      case Both(a, b) =>
        M.append(x(a), b)
    }

  def ===[AA >: A, BB >: B](x: AA \&/ BB)(implicit EA: Equal[AA], EB: Equal[BB]): Boolean =
    this match {
      case This(a) =>
        x match {
          case This(aa) =>
            EA.equal(a, aa)
          case _ =>
            false
        }
      case That(b) =>
        x match {
          case That(bb) =>
            EB.equal(b, bb)
          case _ =>
            false
        }
      case Both(a, b) =>
        x match {
          case Both(aa, bb) =>
            EA.equal(a, aa) && EB.equal(b, bb)
          case _ =>
            false
        }
    }

  def show[AA >: A, BB >: B](implicit SA: Show[AA], SB: Show[BB]): Cord =
    this match {
      case This(a) =>
        "This(" +: SA.show(a) :+ ")"
      case That(b) =>
        "That(" +: SB.show(b) :+ ")"
      case Both(a, b) =>
        ("Both(" +: SA.show(a) :+ ",") ++ SB.show(b) :+ ")"
    }


}

object \&/ extends TheseInstances {
  final case class This[A](aa: A) extends (A \&/ Nothing)
  final case class That[B](bb: B) extends (Nothing \&/ B)
  final case class Both[A, B](aa: A, bb: B) extends (A \&/ B)

  def apply[A, B](a: A, b: B): These[A, B] =
    Both(a, b)

  def unapply[A, B](t: Both[A, B]): Some[(A, B)] =
    Some((t.aa, t.bb))

  import scalaz.std.list._

  def concatThisList[A, B](x: List[A \&/ B]): List[A] =
    concatThis[List, A, B](x)

  def concatThisStream[A, B](x: EphemeralStream[A \&/ B]): EphemeralStream[A] =
    concatThis[EphemeralStream, A, B](x)

  def concatThis[F[_], A, B](x: F[A \&/ B])(implicit M: MonadPlus[F]): F[A] =
    M.bind(x) {
      case This(a) =>
        M.point(a)
      case That(_) =>
        M.empty
      case Both(a, _) =>
        M.point(a)
    }

  def concatThatList[A, B](x: List[A \&/ B]): List[B] =
    concatThat[List, A, B](x)

  def concatThatStream[A, B](x: EphemeralStream[A \&/ B]): EphemeralStream[B] =
    concatThat[EphemeralStream, A, B](x)

  def concatThat[F[_], A, B](x: F[A \&/ B])(implicit M: MonadPlus[F]): F[B] =
    M.bind(x) {
      case This(_) =>
        M.empty
      case That(b) =>
        M.point(b)
      case Both(_, b) =>
        M.point(b)
    }

  def unalignList[A, B](x: List[A \&/ B]): (List[A], List[B]) =
    unalign[List, A, B](x)

  def unalignStream[A, B](x: EphemeralStream[A \&/ B]): (EphemeralStream[A], EphemeralStream[B]) =
    unalign[EphemeralStream, A, B](x)

  def unalign[F[_], A, B](x: F[A \&/ B])(implicit M: MonadPlus[F]): (F[A], F[B]) =
    (concatThis(x), concatThat(x))

  def merge[A](t: A \&/ A)(implicit S: Semigroup[A]): A =
    t match {
      case This(a) =>
        a
      case That(a) =>
        a
      case Both(a1, a2) =>
        S.append(a1, a2)
    }

  @annotation.tailrec
  def tailrecM[L, A, B](f: A => L \&/ (A \/ B))(a: A)(implicit L: Semigroup[L]): L \&/ B = {
    def go(l0: L)(a0: A): L \&/ (A \/ B) =
      f(a0) match {
        case This(l1) => \&/.This(L.append(l0, l1))
        case That(e) => \&/.Both(l0, e)
        case Both(l1, e) => \&/.Both(L.append(l0, l1), e)
      }

    f(a) match {
      case t @ This(l) => t
      case That(-\/(a0)) => tailrecM(f)(a0)
      case That(\/-(b)) => \&/.That(b)
      case Both(l, -\/(a0)) => tailrecM(go(l))(a0)
      case Both(l, \/-(b)) => \&/.Both(l, b)
    }
  }
}

sealed abstract class TheseInstances extends TheseInstances0 {
  type These[A, B] =
  A \&/ B

  implicit def TheseBand[A: Band, B: Band]: Band[A \&/ B] =
    new Band[A \&/ B] {
      def append(f1: A \&/ B, f2: => A \&/ B) =
        f1 append f2
    }
}

sealed abstract class TheseInstances0 extends TheseInstances1 {

  implicit def TheseInstance0[L: Semigroup]: Monad[\&/[L, *]] with BindRec[\&/[L, *]] =
    new Monad[\&/[L, *]] with BindRec[\&/[L, *]] {
      def tailrecM[A, B](f: A => L \&/ (A \/ B))(a: A): L \&/ B =
        \&/.tailrecM(f)(a)

      override def map[A, B](x: L \&/ A)(f: A => B) =
        x map f

      def bind[A, B](fa: L \&/ A)(f: A => L \&/ B) =
        fa flatMap f

      def point[A](a: => A): L \&/ A =
        \&/.That(a)
    }

  implicit val TheseBitraverse: Bitraverse[\&/] =
    new Bitraverse[\&/] {
      override def bimap[A, B, C, D](fab: A \&/ B)(f: A => C, g: B => D) =
        fab.bimap(f, g)

      override def bifoldMap[A, B, M](fa: A \&/ B)(f: A => M)(g: B => M)(implicit F: Monoid[M]) =
        fa.bifoldMap(f)(g)

      override def bifoldRight[A, B, C](fa: A \&/ B, z: => C)(f: (A, => C) => C)(g: (B, => C) => C) =
        fa.bifoldRight(z)(f)(g)

      def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: A \&/ B)(f: A => G[C], g: B => G[D]) =
        fab.bitraverse(f, g)
    }

  implicit final def TheseOrder[A, B](implicit A: Order[A], B: Order[B]): Order[A \&/ B] =
    new Order[A \&/ B] {
      override def equal(x: A \&/ B, y: A \&/ B) =
        x === y
      override def order(x: A \&/ B, y: A \&/ B) = x match {
        case \&/.This(a1) =>
          y match {
            case \&/.This(a2) =>
              A.order(a1, a2)
            case _ =>
              Ordering.GT
          }
        case \&/.That(b1) =>
          y match {
            case \&/.That(b2) =>
              B.order(b1, b2)
            case _ =>
              Ordering.LT
          }
        case \&/.Both(a1, b1) =>
          y match {
            case \&/.Both(a2, b2) =>
              A.order(a1, a2) match {
                case Ordering.EQ => B.order(b1, b2)
                case o => o
              }
            case \&/.This(_) =>
              Ordering.LT
            case \&/.That(_) =>
              Ordering.GT
          }
      }
    }
}

sealed abstract class TheseInstances1 {

  implicit def TheseInstance1[L]: Traverse[\&/[L, *]] with Cobind[\&/[L, *]] =
    new Traverse[\&/[L, *]] with Cobind[\&/[L, *]] {
      def traverseImpl[G[_] : Applicative, A, B](fa: L \&/ A)(f: A => G[B]) =
        fa traverse f

      override def foldMap[A, B](fa: L \&/ A)(f: A => B)(implicit F: Monoid[B]) =
        fa foldMap f

      override def foldRight[A, B](fa: L \&/ A, z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)(f)

      def cobind[A, B](fa: L \&/ A)(f: (L \&/ A) => B): L \&/ B =
        \&/.That(f(fa))
    }

  implicit def TheseEqual[A, B](implicit EA: Equal[A], EB: Equal[B]): Equal[A \&/ B] =
    Equal.equal(_ === _)

  implicit def TheseSemigroup[A, B](implicit SA: Semigroup[A], SB: Semigroup[B]): Semigroup[A \&/ B] =
    Semigroup.instance(_.append(_))

  implicit def TheseShow[A, B](implicit SA: Show[A], SB: Show[B]): Show[A \&/ B] =
    Show.show(_.show)
}

