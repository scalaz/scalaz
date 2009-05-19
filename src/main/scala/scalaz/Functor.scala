package scalaz

trait Functor[F[_]] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {
  import Scalaz._

  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](r: Identity[A], f: A => B) = Identity.IdentityTo(f(r.value))
  }

  implicit def ContinuationFunctor[R] = new Functor[PartialApply1Of2[Continuation, R]#Apply] {
    def fmap[A, B](r: Continuation[R, A], f: A => B) = Continuation.continuation[R, B](k => r(k compose f))
  }

  implicit val NonEmptyListFunctor = new Functor[NonEmptyList] {
    def fmap[A, B](r: NonEmptyList[A], f: A => B) = r map f
  }

  implicit def StateFunctor[S] = new Functor[PartialApply1Of2[State, S]#Apply] {
    def fmap[A, B](r: State[S, A], f: A => B) = r map f
  }

  implicit val ZipStreamFunctor: Functor[ZipStream] = new Functor[ZipStream] {
    def fmap[A, B](r: ZipStream[A], f: A => B) = r.value map f |!|
  }

  implicit val Tuple1Functor = new Functor[Tuple1] {
    def fmap[A, B](r: Tuple1[A], f: A => B) = Tuple1(f(r._1))
  }

  implicit def Tuple2Functor[R] = new Functor[PartialApply1Of2[Tuple2, R]#Apply] {
    def fmap[A, B](r: (R, A), f: A => B) = (r._1, f(r._2))
  }

  implicit def Tuple3Functor[R, S] = new Functor[PartialApply2Of3[Tuple3, R, S]#Apply] {
    def fmap[A, B](r: (R, S, A), f: A => B) = (r._1, r._2, f(r._3))
  }

  implicit def Tuple4Functor[R, S, T] = new Functor[PartialApply3Of4[Tuple4, R, S, T]#Apply] {
    def fmap[A, B](r: (R, S, T, A), f: A => B) = (r._1, r._2, r._3, f(r._4))
  }

  implicit def Tuple5Functor[R, S, T, U] = new Functor[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] {
    def fmap[A, B](r: (R, S, T, U, A), f: A => B) = (r._1, r._2, r._3, r._4, f(r._5))
  }

  implicit def Tuple6Functor[R, S, T, U, V] = new Functor[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V, A), f: A => B) = (r._1, r._2, r._3, r._4, r._5, f(r._6))
  }

  implicit def Tuple7Functor[R, S, T, U, V, W] = new Functor[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V, W, A), f: A => B) = (r._1, r._2, r._3, r._4, r._5, r._6, f(r._7))
  }

  implicit val Function0Functor = new Functor[Function0] {
    def fmap[A, B](r: Function0[A], f: A => B) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit def Function1Functor[R] = new Functor[PartialApply1Of2[Function1, R]#Apply] {
    def fmap[A, B](r: R => A, f: A => B) = r andThen f
  }

  implicit def Function2Functor[R, S] = new Functor[PartialApply2Of3[Function2, R, S]#Apply] {
    def fmap[A, B](r: (R, S) => A, f: A => B) = (t1: R, t2: S) => f(r(t1, t2))
  }

  implicit def Function3Functor[R, S, T] = new Functor[PartialApply3Of4[Function3, R, S, T]#Apply] {
    def fmap[A, B](r: (R, S, T) => A, f: A => B) = (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))
  }

  implicit def Function4Functor[R, S, T, U] = new Functor[PartialApply4Of5[Function4, R, S, T, U]#Apply] {
    def fmap[A, B](r: (R, S, T, U) => A, f: A => B) = (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))
  }

  implicit def Function5Functor[R, S, T, U, V] = new Functor[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V) => A, f: A => B) = (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))
  }

  implicit def Function6Functor[R, S, T, U, V, W] = new Functor[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V, W) => A, f: A => B) = (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))
  }

  implicit val ListFunctor = new Functor[List] {
    def fmap[A, B](r: List[A], f: A => B) = r map f
  }

  implicit val StreamFunctor = new Functor[Stream] {
    def fmap[A, B](r: Stream[A], f: A => B) = r map f
  }

  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](r: Option[A], f: A => B) = r map f
  }

  implicit val ArrayFunctor = new Functor[Array] {
    def fmap[A, B](r: Array[A], f: A => B) = r map f
  }

  implicit def EitherLeftFunctor[X] = new Functor[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def fmap[A, B](r: Either.LeftProjection[A, X], f: A => B) = r.map(f).left
  }

  implicit def EitherRightFunctor[X] = new Functor[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def fmap[A, B](r: Either.RightProjection[X, A], f: A => B) = r.map(f).right
  }

  implicit def ValidationFunctor[X] = new Functor[PartialApply1Of2[Validation, X]#Apply] {
    def fmap[A, B](r: Validation[X, A], f: A => B) = r match {
      case Success(a) => Success(f(a))
      case Failure(e) => Failure(e)
    }
  }

  implicit def ValidationFailureFunctor[X] = new Functor[PartialApply1Of2[Validation.FailureProjection, X]#Flip] {
    def fmap[A, B](r: Validation.FailureProjection[A, X], f: A => B) = (r.validation match {
      case Success(a) => Success(a)
      case Failure(e) => Failure(f(e))
    }).fail
  }

  implicit val ZipperFunctor = new Functor[Zipper] {
    def fmap[A, B](z: Zipper[A], f: A => B) = Zipper.zipper(z.lefts map f, f(z.focus), z.rights map f)
  }

  implicit val TreeFunctor: Functor[Tree] = new Functor[Tree] {
    def fmap[A, B](t: Tree[A], f: A => B): Tree[B] = Tree.node(f(t.rootLabel), t.subForest.map(fmap(_: Tree[A], f)))
  }

  implicit val TreeLocFunctor: Functor[TreeLoc] = new Functor[TreeLoc] {
    def fmap[A, B](t: TreeLoc[A], f: A => B): TreeLoc[B] = {
      val ff = (_: Tree[A]).map(f)
      TreeLoc.loc(t.tree map f, t.lefts map ff, t.rights map ff,
        t.parents.map((ltr) => (ltr._1 map ff, f(ltr._2), ltr._3 map ff)))
    }

  }

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListFunctor: Functor[ArrayList] = new Functor[ArrayList] {
    def fmap[A, B](r: ArrayList[A], f: A => B) = {
      val a = new ArrayList[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaLinkedListFunctor: Functor[LinkedList] = new Functor[LinkedList] {
    def fmap[A, B](r: LinkedList[A], f: A => B) = {
      val a = new LinkedList[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaPriorityQueueFunctor: Functor[PriorityQueue] = new Functor[PriorityQueue] {
    def fmap[A, B](r: PriorityQueue[A], f: A => B) = {
      val a = new PriorityQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaStackFunctor: Functor[Stack] = new Functor[Stack] {
    def fmap[A, B](r: Stack[A], f: A => B) = {
      val a = new Stack[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaVectorFunctor: Functor[Vector] = new Functor[Vector] {
    def fmap[A, B](r: Vector[A], f: A => B) = {
      val a = new Vector[B](r.capacity)
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaArrayBlockingQueueFunctor: Functor[ArrayBlockingQueue] = new Functor[ArrayBlockingQueue] {
    def fmap[A, B](r: ArrayBlockingQueue[A], f: A => B) = {
      val a = new ArrayBlockingQueue[B](r.remainingCapacity)
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }


  implicit val JavaConcurrentLinkedQueueFunctor: Functor[ConcurrentLinkedQueue] = new Functor[ConcurrentLinkedQueue] {
    def fmap[A, B](r: ConcurrentLinkedQueue[A], f: A => B) = {
      val a = new ConcurrentLinkedQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaCopyOnWriteArrayListFunctor: Functor[CopyOnWriteArrayList] = new Functor[CopyOnWriteArrayList] {
    def fmap[A, B](r: CopyOnWriteArrayList[A], f: A => B) = {
      val a = new CopyOnWriteArrayList[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaLinkedBlockingQueueFunctor: Functor[LinkedBlockingQueue] = new Functor[LinkedBlockingQueue] {
    def fmap[A, B](r: LinkedBlockingQueue[A], f: A => B) = {
      val a = new LinkedBlockingQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  implicit val JavaSynchronousQueueFunctor: Functor[SynchronousQueue] = new Functor[SynchronousQueue] {
    def fmap[A, B](r: SynchronousQueue[A], f: A => B) = {
      val a = new SynchronousQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }
}
