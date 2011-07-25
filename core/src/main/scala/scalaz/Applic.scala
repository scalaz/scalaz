package scalaz

import java.util.AbstractMap.SimpleImmutableEntry

trait Applic[F[_]] {
  def applic[A, B](f: F[A => B]): F[A] => F[B]

  def **[G[_] : Applic]: Applic[({type λ[α] = (F[α], G[α])})#λ] =
    new Applic[({type λ[α] = (F[α], G[α])})#λ] {
      def applic[A, B](f: (F[A => B], G[A => B])) = {
        case (a, b) => (Applic.this.applic(f._1)(a), implicitly[Applic[G]].applic(f._2)(b))
      }
    }

  def deriving[G[_]](implicit n: ^**^[G, F]): Applic[G] =
    new Applic[G] {
      def applic[A, B](f: G[A => B]) =
        k => n.pack(Applic.this.applic(n unpack f)(n unpack k))
    }
}

object Applic extends Applics

trait Applics {

  implicit val OptionApplic: Applic[Option] = new Applic[Option] {
    def applic[A, B](f: Option[A => B]) =
      a => f flatMap (a map _)
  }

  implicit val ListApplic: Applic[List] = new Applic[List] {
    def applic[A, B](f: List[A => B]) =
      a => f flatMap (a map _)
  }

  implicit val StreamApplic: Applic[Stream] = new Applic[Stream] {
    def applic[A, B](f: Stream[A => B]) =
      a => f flatMap (a map _)
  }

  implicit def EitherLeftApplic[X]: Applic[({type λ[α] = Either.LeftProjection[α, X]})#λ] =
    new Applic[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
      def applic[A, B](f: Either.LeftProjection[A => B, X]) =
        a => f flatMap (g => (a map g)) left
    }

  implicit def EitherRightApplic[X]: Applic[({type λ[α] = Either.RightProjection[X, α]})#λ] =
    new Applic[({type λ[α] = Either.RightProjection[X, α]})#λ] {
      def applic[A, B](f: Either.RightProjection[X, A => B]) =
        a => f flatMap (a map _) right
    }

  implicit def EitherApplic[X]: Applic[({type λ[α] = Either[X, α]})#λ] =
    new Applic[({type λ[α] = Either[X, α]})#λ] {
      def applic[A, B](f: Either[X, A => B]) =
        a => f.right flatMap (a.right map _)
    }

  import java.util.Map.Entry

  implicit def MapEntryApply[X: Semigroup]: Applic[({type λ[α] = Entry[X, α]})#λ] =
    new Applic[({type λ[α] = Entry[X, α]})#λ] {
      def applic[A, B](f: Entry[X, A => B]) =
        e => new SimpleImmutableEntry[X, B](implicitly[Semigroup[X]].append(f.getKey, e.getKey), f.getValue.apply(e.getValue))

    }

  implicit def Tuple1Applic: Applic[Tuple1] = new Applic[Tuple1] {
    def applic[A, B](f: Tuple1[A => B]) =
      a => Tuple1(f._1(a._1))
  }

  implicit def Tuple2Applic[R: Semigroup]: Applic[({type λ[α] = (R, α)})#λ] = new Applic[({type λ[α] = (R, α)})#λ] {
    def applic[A, B](f: (R, A => B)) = {
      case (r, a) => (implicitly[Semigroup[R]].append(f._1, r), f._2(a))
    }
  }

  implicit def Tuple3Applic[R: Semigroup, S: Semigroup]: Applic[({type λ[α] = (R, S, α)})#λ] = new Applic[({type λ[α] = (R, S, α)})#λ] {
    def applic[A, B](f: (R, S, A => B)) = {
      case (r, s, a) => (implicitly[Semigroup[R]].append(f._1, r), implicitly[Semigroup[S]].append(f._2, s), f._3(a))
    }
  }

  implicit def Tuple4Applic[R: Semigroup, S: Semigroup, T: Semigroup]: Applic[({type λ[α] = (R, S, T, α)})#λ] = new Applic[({type λ[α] = (R, S, T, α)})#λ] {
    def applic[A, B](f: (R, S, T, A => B)) = {
      case (r, s, t, a) => (implicitly[Semigroup[R]].append(f._1, r), implicitly[Semigroup[S]].append(f._2, s), implicitly[Semigroup[T]].append(f._3, t), f._4(a))
    }
  }

  implicit def Tuple5Applic[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup]: Applic[({type λ[α] = (R, S, T, U, α)})#λ] = new Applic[({type λ[α] = (R, S, T, U, α)})#λ] {
    def applic[A, B](f: (R, S, T, U, A => B)) = {
      case (r, s, t, u, a) => (implicitly[Semigroup[R]].append(f._1, r), implicitly[Semigroup[S]].append(f._2, s), implicitly[Semigroup[T]].append(f._3, t), implicitly[Semigroup[U]].append(f._4, u), f._5(a))
    }
  }

  implicit def Tuple6Applic[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup]: Applic[({type λ[α] = (R, S, T, U, V, α)})#λ] = new Applic[({type λ[α] = (R, S, T, U, V, α)})#λ] {
    def applic[A, B](f: (R, S, T, U, V, A => B)) = {
      case (r, s, t, u, v, a) => (implicitly[Semigroup[R]].append(f._1, r), implicitly[Semigroup[S]].append(f._2, s), implicitly[Semigroup[T]].append(f._3, t), implicitly[Semigroup[U]].append(f._4, u), implicitly[Semigroup[V]].append(f._5, v), f._6(a))
    }
  }

  implicit def Tuple7Applic[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup]: Applic[({type λ[α] = (R, S, T, U, V, W, α)})#λ] = new Applic[({type λ[α] = (R, S, T, U, V, W, α)})#λ] {
    def applic[A, B](f: (R, S, T, U, V, W, A => B)) = {
      case (r, s, t, u, v, w, a) => (implicitly[Semigroup[R]].append(f._1, r), implicitly[Semigroup[S]].append(f._2, s), implicitly[Semigroup[T]].append(f._3, t), implicitly[Semigroup[U]].append(f._4, u), implicitly[Semigroup[V]].append(f._5, v), implicitly[Semigroup[W]].append(f._6, w), f._7(a))
    }
  }

  implicit def Function0Applic: Applic[Function0] = new Applic[Function0] {
    def applic[A, B](f: Function0[A => B]) =
      a => () => f.apply.apply(a.apply)
  }

  implicit def Function1Applic[R]: Applic[({type λ[α] = (R) => α})#λ] = new Applic[({type λ[α] = (R) => α})#λ] {
    def applic[A, B](f: Function1[R, A => B]) =
      a => r => f(r)(a(r))
  }

  implicit def Function2Applic[R, S]: Applic[({type λ[α] = (R, S) => α})#λ] = new Applic[({type λ[α] = (R, S) => α})#λ] {
    def applic[A, B](f: Function2[R, S, A => B]) =
      a => (r, s) => f(r, s)(a(r, s))
  }

  implicit def Function3Applic[R, S, T]: Applic[({type λ[α] = (R, S, T) => α})#λ] = new Applic[({type λ[α] = (R, S, T) => α})#λ] {
    def applic[A, B](f: Function3[R, S, T, A => B]) =
      a => (r, s, t) => f(r, s, t)(a(r, s, t))
  }

  implicit def Function4Applic[R, S, T, U]: Applic[({type λ[α] = (R, S, T, U) => α})#λ] = new Applic[({type λ[α] = (R, S, T, U) => α})#λ] {
    def applic[A, B](f: Function4[R, S, T, U, A => B]) =
      a => (r, s, t, u) => f(r, s, t, u)(a(r, s, t, u))
  }

  implicit def Function5Applic[R, S, T, U, V]: Applic[({type λ[α] = (R, S, T, U, V) => α})#λ] = new Applic[({type λ[α] = (R, S, T, U, V) => α})#λ] {
    def applic[A, B](f: Function5[R, S, T, U, V, A => B]) =
      a => (r, s, t, u, v) => f(r, s, t, u, v)(a(r, s, t, u, v))
  }

  implicit def Function6Applic[R, S, T, U, V, W]: Applic[({type λ[α] = (R, S, T, U, V, W) => α})#λ] = new Applic[({type λ[α] = (R, S, T, U, V, W) => α})#λ] {
    def applic[A, B](f: Function6[R, S, T, U, V, W, A => B]) =
      a => (r, s, t, u, v, w) => f(r, s, t, u, v, w)(a(r, s, t, u, v, w))
  }

  implicit def CoKleisliApplic[F[_], R]: Applic[({type λ[α] = CoKleisli[R, F, α]})#λ] = new Applic[({type λ[α] = CoKleisli[R, F, α]})#λ] {
    def applic[A, B](f: CoKleisli[R, F, A => B]) =
      a => f flatMap (a map _)
  }

  implicit def ConstApplic[B: Semigroup]: Applic[({type λ[α] = Const[B, α]})#λ] = new Applic[({type λ[α] = Const[B, α]})#λ] {
    def applic[A, X](f: Const[B, A => X]) =
      fa =>
        Const.const[X](implicitly[Semigroup[B]].append(f.value, fa.value))
  }

  implicit val IdentityApplic: Applic[Identity] = implicitly[Monad[Identity]].applic

  implicit def KleisliApplic[F[_], R](implicit ap: Applic[F]): Applic[({type λ[α] = Kleisli[R, F, α]})#λ] = new Applic[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def applic[A, B](f: Kleisli[R, F, A => B]) =
      a => Kleisli.kleisli(r =>
        ap.applic(f.run(r))(a.run(r)))
  }

  implicit val NonEmptyListApplic: Applic[NonEmptyList] = new Applic[NonEmptyList] {
    def applic[A, B](f: NonEmptyList[A => B]) =
      r =>
        for {
          ff <- f
          rr <- r
        } yield ff(rr)
  }

  implicit def ReaderWriterStateTApplic[R, W: Semigroup, S, F[_] : BindFunctor]: Applic[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ] = new Applic[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ] {
    def applic[X, Y](f: ReaderWriterStateT[R, W, S, F, X => Y]) = {
      implicit val ftr = implicitly[BindFunctor[F]].functor
      r =>
        for {
          ff <- f
          rr <- r
        } yield ff(rr)
    }
  }

  implicit def StateTApplic[A, F[_] : BindFunctor]: Applic[({type λ[α] = StateT[A, F, α]})#λ] = new Applic[({type λ[α] = StateT[A, F, α]})#λ] {
    def applic[X, Y](f: StateT[A, F, X => Y]) =
      implicitly[BindFunctor[({type λ[α] = StateT[A, F, α]})#λ]].liftA2[X => Y, X, Y](identity)(f)
  }

  implicit def StepListTApplic[F[_] : Functor]: Applic[({type λ[X] = StepListT[F, X]})#λ] = new Applic[({type λ[X] = StepListT[F, X]})#λ] {
    def applic[A, B](f: StepListT[F, A => B]) =
      a =>
        for {
          ff <- f
          aa <- a
        } yield ff(aa)
  }

  implicit def StepStreamTApplic[F[_] : Functor]: Applic[({type λ[X] = StepStreamT[F, X]})#λ] = new Applic[({type λ[X] = StepStreamT[F, X]})#λ] {
    def applic[A, B](f: StepStreamT[F, A => B]) =
      a =>
        for {
          ff <- f
          aa <- a
        } yield ff(aa)
  }

  implicit val TreeApplic: Applic[Tree] = new Applic[Tree] {

    import wrap.StreamW._

    def applic[A, B](f: Tree[A => B]) =
      a =>
        Tree.node((f.rootLabel)(a.rootLabel), implicitly[Applic[newtypes.ZipStream]].applic(f.subForest.map(applic[A, B](_)).ʐ)(a.subForest ʐ).value)
  }

  implicit def FailProjectionApplic[X]: Applic[({type λ[α] = FailProjection[α, X]})#λ] =
    new Applic[({type λ[α] = FailProjection[α, X]})#λ] {
      def applic[A, B](f: FailProjection[A => B, X]) =
        a =>
          ((f.validation, a.validation) match {
            case (Success(x1), Success(_)) => Success[B, X](x1)
            case (Success(x1), Failure(_)) => Success[B, X](x1)
            case (Failure(_), Success(x2)) => Success[B, X](x2)
            case (Failure(f), Failure(e)) => Failure[B, X](f(e))
          }).fail
    }

  implicit def ValidationApplic[X: Semigroup]: Applic[({type λ[α] = Validation[X, α]})#λ] = new Applic[({type λ[α] = Validation[X, α]})#λ] {
    def applic[A, B](f: Validation[X, A => B]) =
      a => (f, a) match {
        case (Success(f), Success(a)) => Validation.success(f(a))
        case (Success(_), Failure(e)) => Validation.failure(e)
        case (Failure(e), Success(_)) => Validation.failure(e)
        case (Failure(e1), Failure(e2)) => Validation.failure(implicitly[Semigroup[X]].append(e1, e2))
      }
  }

  implicit def WriterTApplic[A: Semigroup, F[_] : ApplicFunctor]: Applic[({type λ[α] = WriterT[A, F, α]})#λ] = new Applic[({type λ[α] = WriterT[A, F, α]})#λ] {
    def applic[X, Y](f: WriterT[A, F, X => Y]) =
      a =>
        WriterT.writerT(implicitly[ApplicFunctor[F]].liftA2((ff: (A, X => Y)) => (xx: (A, X)) => (implicitly[Semigroup[A]].append(ff._1, xx._1), ff._2(xx._2)))(f.runT)(a.runT))
  }

  implicit def ZipperApplic: Applic[Zipper] = new Applic[Zipper] {
    def applic[A, B](f: Zipper[A => B]) =
      a =>
        Zipper.zipper(a.lefts.zip(f.lefts) map {
          case (aa, ff) => ff(aa)
        },
          (f.focus)(a.focus),
          a.rights.zip(f.rights) map {
            case (aa, ff) => ff(aa)
          })
  }

  implicit def OptionTApplic[F[_] : ApplicFunctor]: Applic[({type λ[α] = OptionT[F, α]})#λ] = new Applic[({type λ[α] = OptionT[F, α]})#λ] {
    def applic[A, B](f: OptionT[F, A => B]) =
      a =>
        OptionT.optionT(implicitly[ApplicFunctor[F]].liftA2((ff: Option[A => B]) => (aa: Option[A]) => implicitly[Applic[Option]].applic(ff)(aa))(f.runT)(a.runT))
  }

  implicit def LazyOptionTApplic[F[_] : ApplicFunctor]: Applic[({type λ[α] = LazyOptionT[F, α]})#λ] = new Applic[({type λ[α] = LazyOptionT[F, α]})#λ] {
    def applic[A, B](f: LazyOptionT[F, A => B]) =
      a =>
        LazyOptionT.lazyOptionT(implicitly[ApplicFunctor[F]].liftA2((ff: LazyOption[A => B]) => (aa: LazyOption[A]) => implicitly[Applic[LazyOption]].applic(ff)(aa))(f.runT)(a.runT))
  }

  implicit def EitherTApplic[F[_] : ApplicFunctor, X]: Applic[({type λ[α] = EitherT[X, F, α]})#λ] = new Applic[({type λ[α] = EitherT[X, F, α]})#λ] {
    def applic[A, B](f: EitherT[X, F, A => B]) =
      a =>
        EitherT.eitherT[X, F, B](implicitly[ApplicFunctor[F]].liftA2((ff: Either[X, A => B]) => (aa: Either[X, A]) => implicitly[Applic[({type λ[α] = Either[X, α]})#λ]].applic(ff)(aa))(f.runT)(a.runT))
  }

  implicit def LeftEitherTApplic[F[_] : ApplicFunctor, X]: Applic[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] = new Applic[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] {
    def applic[A, B](f: EitherT.LeftProjectionT[A => B, F, X]) =
      a =>
        EitherT.eitherT[B, F, X](implicitly[ApplicFunctor[F]].liftA2((ff: Either[A => B, X]) => (aa: Either[A, X]) => implicitly[Applic[({type λ[α] = Either.LeftProjection[α, X]})#λ]].applic(ff.left)(aa.left).e)(f.e.runT)(a.e.runT)).left
  }

  implicit def LazyEitherTApplic[F[_] : ApplicFunctor, X]: Applic[({type λ[α] = LazyEitherT[X, F, α]})#λ] = new Applic[({type λ[α] = LazyEitherT[X, F, α]})#λ] {
    def applic[A, B](f: LazyEitherT[X, F, A => B]) =
      a =>
        LazyEitherT.lazyEitherT[X, F, B](implicitly[ApplicFunctor[F]].liftA2((ff: LazyEither[X, A => B]) => (aa: LazyEither[X, A]) => implicitly[Applic[({type λ[α] = LazyEither[X, α]})#λ]].applic(ff)(aa))(f.runT)(a.runT))
  }

  implicit def LazyLeftEitherTApplic[F[_] : ApplicFunctor, X]: Applic[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] = new Applic[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] {
    def applic[A, B](f: LazyEitherT.LazyLeftProjectionT[A => B, F, X]) =
      a =>
        LazyEitherT.lazyEitherT[B, F, X](implicitly[ApplicFunctor[F]].liftA2((ff: LazyEither[A => B, X]) => (aa: LazyEither[A, X]) => implicitly[Applic[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ]].applic(ff.left)(aa.left).e)(f.e.runT)(a.e.runT)).left
  }

  implicit val LazyOptionApplic: Applic[LazyOption] = new Applic[LazyOption] {
    def applic[A, B](f: LazyOption[A => B]) =
      a => f flatMap (k => a map (k apply _))
  }

  implicit def LazyEitherApplic[X]: Applic[({type λ[α] = LazyEither[X, α]})#λ] = new Applic[({type λ[α] = LazyEither[X, α]})#λ] {
    def applic[A, B](f: LazyEither[X, A => B]) =
      a => f flatMap (k => a map (k apply _))
  }

  implicit def LazyLeftEitherApplicFunctor[X]: Applic[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] = new Applic[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] {
    def applic[A, B](f: LazyEither.LazyLeftProjection[A => B, X]) =
      a => f flatMap (k => a map (k apply _)) left
  }

  import scala.util.control.TailCalls
  import TailCalls.TailRec

  implicit def TailRecApplic: Applic[TailRec] = new Applic[TailRec] {
    def applic[A, B](f: TailRec[A => B]): TailRec[A] => TailRec[B] =
      a => TailCalls.done(f.result(a.result))
  }

  import scala.util.continuations.ControlContext

  implicit def ControlContextApplic[B]: Applic[({type T[A] = ControlContext[A, B, B]})#T] = new Applic[({type T[A] = ControlContext[A, B, B]})#T] {
    def applic[A, A1](f: ControlContext[A => A1, B, B]): ControlContext[A, B, B] => ControlContext[A1, B, B] =
      a =>
        for {
          ff <- f
          aa <- a
        } yield ff(aa)
  }

  implicit val ResponderApplic =
    implicitly[Functor[Responder]].applicBind

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListApplic =
    implicitly[Functor[ArrayList]].applicBind

  implicit val JavaLinkedListApplic =
    implicitly[Functor[LinkedList]].applicBind

  implicit val JavaPriorityQueueApplic =
    implicitly[Functor[PriorityQueue]].applicBind

  implicit val JavaStackApplic =
    implicitly[Functor[Stack]].applicBind

  implicit val JavaVectorApplic =
    implicitly[Functor[Vector]].applicBind

  implicit val JavaArrayBlockingQueueApplic =
    implicitly[Functor[ArrayBlockingQueue]].applicBind

  implicit val JavaConcurrentLinkedQueueApplic =
    implicitly[Functor[ConcurrentLinkedQueue]].applicBind

  implicit val JavaCopyOnWriteArrayListApplic =
    implicitly[Functor[CopyOnWriteArrayList]].applicBind

  implicit val JavaLinkedBlockingQueueApplic =
    implicitly[Functor[LinkedBlockingQueue]].applicBind

  implicit val JavaSynchronousQueueApplic =
    implicitly[Functor[SynchronousQueue]].applicBind
}
