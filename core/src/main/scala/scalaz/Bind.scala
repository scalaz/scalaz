package scalaz

trait Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]

  def deriving[G[_]](implicit n: ^**^[G, F]): Bind[G] =
    new Bind[G] {
      def bind[A, B](f: A => G[B]) =
        k => n.pack(Bind.this.bind((a: A) => n.unpack(f(a)))(n unpack k))
    }
}

object Bind extends Binds

trait Binds extends BindsLow {
  implicit val OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](f: A => Option[B]) =
      _ flatMap f
  }

  implicit val ListBind: Bind[List] = new Bind[List] {
    def bind[A, B](f: A => List[B]) =
      _ flatMap f
  }

  implicit val StreamBind: Bind[Stream] = new Bind[Stream] {
    def bind[A, B](f: A => Stream[B]) =
      _ flatMap f
  }

  implicit def EitherLeftBind[X]: Bind[({type λ[α] = Either.LeftProjection[α, X]})#λ] = new Bind[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
    def bind[A, B](f: A => Either.LeftProjection[B, X]) =
      _.flatMap(f(_).e).left
  }

  implicit def EitherRightBind[X]: Bind[({type λ[α] = Either.RightProjection[X, α]})#λ] = new Bind[({type λ[α] = Either.RightProjection[X, α]})#λ] {
    def bind[A, B](f: A => Either.RightProjection[X, B]) =
      _.flatMap(f(_).e).right
  }

  implicit def EitherBind[X]: Bind[({type λ[α] = Either[X, α]})#λ] = new Bind[({type λ[α] = Either[X, α]})#λ] {
    def bind[A, B](f: A => Either[X, B]) =
      _.fold(Left(_), f)
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryBind[X: Semigroup]: Bind[({type λ[α] = Entry[X, α]})#λ] =
    new Bind[({type λ[α] = Entry[X, α]})#λ] {
      def bind[A, B](f: A => Entry[X, B]) =
        r => {
          val e = f(r.getValue)
          new SimpleImmutableEntry(implicitly[Semigroup[X]].append(r.getKey, e.getKey), e.getValue)
        }
    }

  implicit def Tuple1Bind: Bind[Tuple1] = new Bind[Tuple1] {
    def bind[A, B](f: A => Tuple1[B]) =
      r => f(r._1)
  }

  implicit def Tuple2Bind[R: Semigroup]: Bind[({type λ[α] = (R, α)})#λ] = new Bind[({type λ[α] = (R, α)})#λ] {
    def bind[A, B](f: A => (R, B)) = r => {
      val (rr, b) = f(r._2)
      (implicitly[Semigroup[R]].append(r._1, rr), b)

    }
  }

  implicit def Tuple3Bind[R: Semigroup, S: Semigroup]: Bind[({type λ[α] = (R, S, α)})#λ] = new Bind[({type λ[α] = (R, S, α)})#λ] {
    def bind[A, B](f: A => (R, S, B)) = r => {
      val (rr, s, b) = f(r._3)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), b)
    }
  }

  implicit def Tuple4Bind[R: Semigroup, S: Semigroup, T: Semigroup]: Bind[({type λ[α] = (R, S, T, α)})#λ] = new Bind[({type λ[α] = (R, S, T, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, B)) = r => {
      val (rr, s, t, b) = f(r._4)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), b)
    }
  }

  implicit def Tuple5Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup]: Bind[({type λ[α] = (R, S, T, U, α)})#λ] = new Bind[({type λ[α] = (R, S, T, U, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, U, B)) = r => {
      val (rr, s, t, u, b) = f(r._5)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), implicitly[Semigroup[U]].append(r._4, u), b)
    }
  }

  implicit def Tuple6Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup]: Bind[({type λ[α] = (R, S, T, U, V, α)})#λ] = new Bind[({type λ[α] = (R, S, T, U, V, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V, B)) = r => {
      val (rr, s, t, u, v, b) = f(r._6)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), implicitly[Semigroup[U]].append(r._4, u), implicitly[Semigroup[V]].append(r._5, v), b)
    }
  }

  implicit def Tuple7Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup]: Bind[({type λ[α] = (R, S, T, U, V, W, α)})#λ] = new Bind[({type λ[α] = (R, S, T, U, V, W, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V, W, B)) = r => {
      val (rr, s, t, u, v, w, b) = f(r._7)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), implicitly[Semigroup[U]].append(r._4, u), implicitly[Semigroup[V]].append(r._5, v), implicitly[Semigroup[W]].append(r._6, w), b)
    }
  }

  implicit def Function0Bind: Bind[Function0] = new Bind[Function0] {
    def bind[A, B](f: A => (() => B)) = r => f(r.apply)
  }

  implicit def Function1Bind[R]: Bind[({type λ[α] = (R) => α})#λ] = new Bind[({type λ[α] = (R) => α})#λ] {
    def bind[A, B](f: A => R => B) = r => (t: R) => f(r(t))(t)
  }

  implicit def Function2Bind[R, S]: Bind[({type λ[α] = (R, S) => α})#λ] = new Bind[({type λ[α] = (R, S) => α})#λ] {
    def bind[A, B](f: A => (R, S) => B) = r => (t1: R, t2: S) => f(r(t1, t2))(t1, t2)
  }

  implicit def Function3Bind[R, S, T]: Bind[({type λ[α] = (R, S, T) => α})#λ] = new Bind[({type λ[α] = (R, S, T) => α})#λ] {
    def bind[A, B](f: A => (R, S, T) => B) = r => (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))(t1, t2, t3)
  }

  implicit def Function4Bind[R, S, T, U]: Bind[({type λ[α] = (R, S, T, U) => α})#λ] = new Bind[({type λ[α] = (R, S, T, U) => α})#λ] {
    def bind[A, B](f: A => (R, S, T, U) => B) = r => (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))(t1, t2, t3, t4)
  }

  implicit def Function5Bind[R, S, T, U, V]: Bind[({type λ[α] = (R, S, T, U, V) => α})#λ] = new Bind[({type λ[α] = (R, S, T, U, V) => α})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V) => B) = r => (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }

  implicit def Function6Bind[R, S, T, U, V, W]: Bind[({type λ[α] = (R, S, T, U, V, W) => α})#λ] = new Bind[({type λ[α] = (R, S, T, U, V, W) => α})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V, W) => B) = r => (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListBind: Bind[ArrayList] = new Bind[ArrayList] {
    def bind[A, B](f: A => ArrayList[B]) = r => {
      val a = new ArrayList[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaLinkedListBind: Bind[LinkedList] = new Bind[LinkedList] {
    def bind[A, B](f: A => LinkedList[B]) = r => {
      val a = new LinkedList[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaPriorityQueueBind: Bind[PriorityQueue] = new Bind[PriorityQueue] {
    def bind[A, B](f: A => PriorityQueue[B]) = r => {
      val a = new PriorityQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaStackBind: Bind[Stack] = new Bind[Stack] {
    def bind[A, B](f: A => Stack[B]) = r => {
      val a = new Stack[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaVectorBind: Bind[Vector] = new Bind[Vector] {
    def bind[A, B](f: A => Vector[B]) = r => {
      val a = new Vector[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaArrayBlockingQueueBind: Bind[ArrayBlockingQueue] = new Bind[ArrayBlockingQueue] {
    def bind[A, B](f: A => ArrayBlockingQueue[B]) = r => {
      val a = new ArrayBlockingQueue[B](r.remainingCapacity)
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaConcurrentLinkedQueueBind: Bind[ConcurrentLinkedQueue] = new Bind[ConcurrentLinkedQueue] {
    def bind[A, B](f: A => ConcurrentLinkedQueue[B]) = r => {
      val a = new ConcurrentLinkedQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaCopyOnWriteArrayListBind: Bind[CopyOnWriteArrayList] = new Bind[CopyOnWriteArrayList] {
    def bind[A, B](f: A => CopyOnWriteArrayList[B]) = r => {
      val a = new CopyOnWriteArrayList[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaLinkedBlockingQueueBind: Bind[LinkedBlockingQueue] = new Bind[LinkedBlockingQueue] {
    def bind[A, B](f: A => LinkedBlockingQueue[B]) = r => {
      val a = new LinkedBlockingQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaSynchronousQueueBind: Bind[SynchronousQueue] = new Bind[SynchronousQueue] {
    def bind[A, B](f: A => SynchronousQueue[B]) = r => {
      val a = new SynchronousQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](f: A => Identity[B]) = a => Identity.id(f(a.value).value)
  }

  implicit def KleisliBind[F[_], R](implicit bd: Bind[F]): Bind[({type λ[α] = Kleisli[R, F, α]})#λ] = new Bind[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def bind[A, B](f: A => Kleisli[R, F, B]) =
      _ flatMap f
  }

  implicit val NonEmptyListBind: Bind[NonEmptyList] = new Bind[NonEmptyList] {
    def bind[A, B](f: A => NonEmptyList[B]) =
      _ flatMap f
  }

  implicit def StateTBind[A, F[_] : Bind]: Bind[({type λ[α] = StateT[A, F, α]})#λ] = new Bind[({type λ[α] = StateT[A, F, α]})#λ] {
    def bind[X, Y](f: X => StateT[A, F, Y]) =
      _ flatMap f
  }

  implicit def StepListTBind[F[_] : Functor]: Bind[({type λ[X] = StepListT[F, X]})#λ] = new Bind[({type λ[X] = StepListT[F, X]})#λ] {
    def bind[A, B](f: A => StepListT[F, B]) =
      _ flatMap f
  }

  implicit def StepStreamTBind[F[_] : Functor]: Bind[({type λ[X] = StepStreamT[F, X]})#λ] = new Bind[({type λ[X] = StepStreamT[F, X]})#λ] {
    def bind[A, B](f: A => StepStreamT[F, B]) =
      _ flatMap f
  }

  implicit def TreeBind: Bind[Tree] = new Bind[Tree] {
    def bind[A, B](f: A => Tree[B]) =
      t => {
        val r = f(t.rootLabel)
        Tree.node(r.rootLabel, r.subForest #::: t.subForest.map(bind(f): Tree[A] => Tree[B]) )
      }
  }

  implicit def FailProjectionBind[X]: Bind[({type λ[α] = FailProjection[α, X]})#λ] =
    new Bind[({type λ[α] = FailProjection[α, X]})#λ] {
      def bind[A, B](f: A => FailProjection[B, X]) =
        r => r.validation match {
          case Success(a) => Success[B, X](a).fail
          case Failure(e) => f(e)
        }
    }

  implicit def WriterTBind[A: Semigroup, F[_] : BindFunctor]: Bind[({type λ[α] = WriterT[A, F, α]})#λ] = new Bind[({type λ[α] = WriterT[A, F, α]})#λ] {
    def bind[X, Y](f: X => WriterT[A, F, Y]) =
      _ flatMap f
  }

}

trait BindsLow {

  implicit def TraversableBind[CC[X] <: collection.TraversableLike[X, CC[X]] with Traversable[X] : CanBuildAnySelf]: Bind[CC] = new Bind[CC] {
    def bind[A, B](f: A => CC[B]) = r => {
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[A, B]
      r.flatMap[B, CC[B]](f)(cbf)
    }
  }
}