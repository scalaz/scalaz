package scalaz
package std

sealed trait EitherInstances0 {
  implicit def eitherEqual[A, B](implicit A0: Equal[A], B0: Equal[B]): Equal[Either[A, B]] = new EitherEqual[A, B] {
    implicit def A = A0
    implicit def B = B0
  }
}

trait EitherInstances extends EitherInstances0 {
  implicit val eitherInstance: Bitraverse[Either] = new Bitraverse[Either] {
    override def bimap[A, B, C, D](fab: Either[A, B])
                                  (f: A => C, g: B => D) = fab match {
      case Left(a)  => Left(f(a))
      case Right(b) => Right(g(b))
    }

    def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: Either[A, B])
                                                  (f: A => G[C], g: B => G[D]) = fab match {
      case Left(a)  => Applicative[G].map(f(a))(b => Left(b))
      case Right(b) => Applicative[G].map(g(b))(d => Right(d))
    }
  }

  /** Right biased monad */
  implicit def eitherMonad[L]: Traverse[Either[L, *]] with MonadError[Either[L, *], L] with BindRec[Either[L, *]] with Cozip[Either[L, *]] with Optional[Either[L, *]] =
    new Traverse[Either[L, *]] with MonadError[Either[L, *], L] with BindRec[Either[L, *]] with Cozip[Either[L, *]] with Optional[Either[L, *]] {
      def bind[A, B](fa: Either[L, A])(f: A => Either[L, B]) = fa match {
        case Left(a)  => Left(a)
        case Right(b) => f(b)
      }

      override def map[A, B](fa: Either[L, A])(f: A => B) = fa match {
        case Right(b) => Right(f(b))
        case a => a.asInstanceOf[Either[L, B]]
      }

      override def apply2[A, B, C](fa: => Either[L, A], fb: => Either[L, B])(f: (A, B) => C): Either[L, C] =
        fa match {
          case Right(a) =>
            fb match {
              case Right(b) => Right(f(a, b))
              case e => e.asInstanceOf[Either[L, C]]
            }
          case e => e.asInstanceOf[Either[L, C]]
        }

      def handleError[A](fa: Either[L, A])(f: L => Either[L, A]) =
        fa match {
          case a @ Right(_) => a
          case Left(a) => f(a)
        }

      def raiseError[A](e: L): Either[L, A] =
        Left(e)

      def point[A](a: => A): Either[L, A] = Right(a)

      def traverseImpl[G[_] : Applicative, A, B](fa: Either[L, A])(f: A => G[B]) = fa match {
        case Left(x)  => Applicative[G].point(Left(x))
        case Right(x) => Applicative[G].map(f(x))(Right(_))
      }

      override def foldRight[A, B](fa: Either[L, A], z: => B)(f: (A, => B) => B) = fa match {
        case Left(_)  => z
        case Right(a) => f(a, z)
      }

      def cozip[A, B](a: Either[L, A \/ B]) =
        a match {
          case Left(l) => -\/(Left(l))
          case Right(e)=> e match {
            case -\/(a) => -\/(Right(a))
            case \/-(b) => \/-(Right(b))
          }
        }

      @scala.annotation.tailrec
      def tailrecM[A, B](a: A)(f: A => Either[L, A \/ B]): Either[L, B] =
        f(a) match {
          case Left(l) => Left(l)
          case Right(-\/(a)) => tailrecM(a)(f)
          case Right(\/-(b)) => Right(b)
        }

      override def pextract[B, A](fa: Either[L, A]): Either[L, B] \/ A =
        fa match {
          case Left(l) =>
            -\/(Left(l))
          case Right(a) =>
            \/-(a)
        }

      override def unfoldrOpt[S, A, B](seed: S)(f: S => Maybe[(Either[L, A], S)])(implicit r: Reducer[A, B]): Maybe[Either[L, B]] = {
        @annotation.tailrec
        def go(acc: B, s1: S): Either[L, B] = f(s1) match {
          case Maybe.Just((ma, s2)) =>
            ma match {
              case Right(a) =>
                go(r.snoc(acc, a), s2)
              case Left(l) =>
                Left(l)
            }
          case _ =>
            Right(acc)
        }
        f(seed).map {
          case (Right(a), s) =>
            go(r.unit(a), s)
          case (Left(l), _) =>
            Left(l)
        }
      }
    }

  implicit def eitherOrder[A, B](implicit OrderA: Order[A], OrderB: Order[B]): Order[Either[A, B]] =
    new EitherOrder[A, B] {
      implicit def A = OrderA
      implicit def B = OrderB
    }

  implicit def eitherAssociative: Associative[Either] = new Associative[Either] {
    override def reassociateLeft[A, B, C](f: Either[A, Either[B, C]]): Either[Either[A, B], C] =
      f.fold(
        a => Left(Left(a)),
        _.fold(
          b => Left(Right(b)),
          Right(_)
        )
      )

    override def reassociateRight[A, B, C](f: Either[Either[A, B], C]): Either[A, Either[B, C]] =
      f.fold(
        _.fold(
          Left(_),
          b => Right(Left(b))
        ),
        c => Right(Right(c))
      )

  }

  implicit def eitherShow[A,B](implicit SA: Show[A], SB: Show[B]) : Show[Either[A,B]] = {
    import scalaz.syntax.show._
    Show.show {
      case Left(a) => cord"Left($a)"
      case Right(b) => cord"Right($b)"
    }
  }
}

object either extends EitherInstances

private trait EitherEqual[A, B] extends Equal[Either[A, B]] {
  implicit def A: Equal[A]
  implicit def B: Equal[B]

  final override def equal(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
    case (Left(a1), Left(a2))                      => A.equal(a1, a2)
    case (Right(b1), Right(b2))                    => B.equal(b1, b2)
    case (Right(_), Left(_)) | (Left(_), Right(_)) => false
  }
  override val equalIsNatural: Boolean = A.equalIsNatural && B.equalIsNatural
}

private trait EitherOrder[A, B] extends Order[Either[A, B]] with EitherEqual[A, B]{
  implicit def A: Order[A]
  implicit def B: Order[B]

  import Ordering._

  def order(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
    case (Left(x), Left(y))   => A.order(x, y)
    case (Right(x), Right(y)) => B.order(x, y)
    case (Left(_), Right(_))  => LT
    case (Right(_), Left(_))  => GT
  }
}
