package scalaz

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  import Scalaz._

  implicit def IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a.value)
  }

  implicit def NonEmptyListBind: Bind[NonEmptyList] = new Bind[NonEmptyList] {
    def bind[A, B](r: NonEmptyList[A], f: A => NonEmptyList[B]) = r flatMap f
  }

  implicit def StateBind[S]: Bind[({type λ[α]=State[S, α]})#λ] = new Bind[({type λ[α]=State[S, α]})#λ] {
    def bind[A, B](r: State[S, A], f: A => State[S, B]) = r flatMap f
  }

  implicit def StateTBind[M[_]:Bind, S]: Bind[({type λ[α]=StateT[M, S, α]})#λ] = new Bind[({type λ[α]=StateT[M, S, α]})#λ] {
    def bind[A, B](r: StateT[M, S, A], f: A => StateT[M, S, B]) = r flatMap f
  }

  implicit def TraversableBind[CC[X] <: collection.TraversableLike[X, CC[X]] with Traversable[X] : CanBuildAnySelf]: Bind[CC] = new Bind[CC] {
    def bind[A, B](r: CC[A], f: A => CC[B]) = {
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[A, B]
      r.flatMap[B, CC[B]](f)(cbf)
    }
  }

  implicit def Tuple1Bind: Bind[Tuple1] = new Bind[Tuple1] {
    def bind[A, B](r: Tuple1[A], f: A => Tuple1[B]) = f(r._1)
  }

  implicit def Tuple2Bind[R: Semigroup]: Bind[({type λ[α]=(R, α)})#λ] = new Bind[({type λ[α]=(R, α)})#λ] {
    def bind[A, B](r: (R, A), f: A => (R, B)) = {
      val (rr, b) = f(r._2)
      (r._1 ⊹ rr, b)
    }
  }

  implicit def Tuple3Bind[R: Semigroup, S: Semigroup]: Bind[({type λ[α]=(R, S, α)})#λ] = new Bind[({type λ[α]=(R, S, α)})#λ] {
    def bind[A, B](r: (R, S, A), f: A => (R, S, B)) = {
      val (rr, s, b) = f(r._3)
      (r._1 ⊹ rr, r._2 ⊹ s, b)
    }
  }

  implicit def Tuple4Bind[R: Semigroup, S: Semigroup, T: Semigroup]: Bind[({type λ[α]=(R, S, T, α)})#λ] = new Bind[({type λ[α]=(R, S, T, α)})#λ] {
    def bind[A, B](r: (R, S, T, A), f: A => (R, S, T, B)) = {
      val (rr, s, t, b) = f(r._4)
      (r._1 ⊹ rr, r._2 ⊹ s, r._3 ⊹ t, b)
    }
  }

  implicit def Tuple5Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup]: Bind[({type λ[α]=(R, S, T, U, α)})#λ] = new Bind[({type λ[α]=(R, S, T, U, α)})#λ] {
    def bind[A, B](r: (R, S, T, U, A), f: A => (R, S, T, U, B)) = {
      val (rr, s, t, u, b) = f(r._5)
      (r._1 ⊹ rr, r._2 ⊹ s, r._3 ⊹ t, r._4 ⊹ u, b)
    }
  }

  implicit def Tuple6Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup]: Bind[({type λ[α]=(R, S, T, U, V, α)})#λ] = new Bind[({type λ[α]=(R, S, T, U, V, α)})#λ] {
    def bind[A, B](r: (R, S, T, U, V, A), f: A => (R, S, T, U, V, B)) = {
      val (rr, s, t, u, v, b) = f(r._6)
      (r._1 ⊹ rr, r._2 ⊹ s, r._3 ⊹ t, r._4 ⊹ u, r._5 ⊹ v, b)
    }
  }

  implicit def Tuple7Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup]: Bind[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = new Bind[({type λ[α]=(R, S, T, U, V, W, α)})#λ] {
    def bind[A, B](r: (R, S, T, U, V, W, A), f: A => (R, S, T, U, V, W, B)) = {
      val (rr, s, t, u, v, w, b) = f(r._7)
      (r._1 ⊹ rr, r._2 ⊹ s, r._3 ⊹ t, r._4 ⊹ u, r._5 ⊹ v, r._6 ⊹ w, b)
    }
  }

  implicit def Function0Bind: Bind[Function0] = new Bind[Function0] {
    def bind[A, B](r: () => A, f: A => (() => B)) = f(r.apply)
  }

  implicit def Function1Bind[R]: Bind[({type λ[α]=(R) => α})#λ] = new Bind[({type λ[α]=(R) => α})#λ] {
    def bind[A, B](r: R => A, f: A => R => B) = (t: R) => f(r(t))(t)
  }

  implicit def Function2Bind[R, S]: Bind[({type λ[α]=(R, S) => α})#λ] = new Bind[({type λ[α]=(R, S) => α})#λ] {
    def bind[A, B](r: (R, S) => A, f: A => (R, S) => B) = (t1: R, t2: S) => f(r(t1, t2))(t1, t2)
  }

  implicit def Function3Bind[R, S, T]: Bind[({type λ[α]=(R, S, T) => α})#λ] = new Bind[({type λ[α]=(R, S, T) => α})#λ] {
    def bind[A, B](r: (R, S, T) => A, f: A => (R, S, T) => B) = (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))(t1, t2, t3)
  }

  implicit def Function4Bind[R, S, T, U]: Bind[({type λ[α]=(R, S, T, U) => α})#λ] = new Bind[({type λ[α]=(R, S, T, U) => α})#λ] {
    def bind[A, B](r: (R, S, T, U) => A, f: A => (R, S, T, U) => B) = (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))(t1, t2, t3, t4)
  }

  implicit def Function5Bind[R, S, T, U, V]: Bind[({type λ[α]=(R, S, T, U, V) => α})#λ] = new Bind[({type λ[α]=(R, S, T, U, V) => α})#λ] {
    def bind[A, B](r: (R, S, T, U, V) => A, f: A => (R, S, T, U, V) => B) = (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }

  implicit def Function6Bind[R, S, T, U, V, W]: Bind[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = new Bind[({type λ[α]=(R, S, T, U, V, W) => α})#λ] {
    def bind[A, B](r: (R, S, T, U, V, W) => A, f: A => (R, S, T, U, V, W) => B) = (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }

  implicit def OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](r: Option[A], f: A => Option[B]) = r flatMap f
  }

  implicit def FirstOptionBind: Bind[FirstOption] = new Bind[FirstOption] {
    def bind[A, B](a: FirstOption[A], f: (A) => FirstOption[B]): FirstOption[B] = (a.value flatMap ((x: A) => f(x).value)).fst
  }

  implicit def LastOptionBind: Bind[LastOption] = new Bind[LastOption] {
    def bind[A, B](a: LastOption[A], f: (A) => LastOption[B]): LastOption[B] = (a.value flatMap ((x: A) => f(x).value)).lst
  } 

  implicit def EitherLeftBind[X]: Bind[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Bind[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def bind[A, B](r: Either.LeftProjection[A, X], f: A => Either.LeftProjection[B, X]) = r.flatMap(f(_).e).left
  }

  implicit def EitherRightBind[X]: Bind[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Bind[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def bind[A, B](r: Either.RightProjection[X, A], f: A => Either.RightProjection[X, B]) = r.flatMap(f(_).e).right
  }

  implicit def ResponderBind: Bind[Responder] = new Bind[Responder] {
    def bind[A, B](r: Responder[A], f: A => Responder[B]) = r flatMap f
  }

  import java.util.concurrent.Callable

  implicit def CallableBind: Bind[Callable] = new Bind[Callable] {
    def bind[A, B](r: Callable[A], f: A => Callable[B]) = f(r.call)
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryBind[X: Semigroup]: Bind[({type λ[α]=Entry[X, α]})#λ] = new Bind[({type λ[α]=Entry[X, α]})#λ] {
    def bind[A, B](r: Entry[X, A], f: A => Entry[X, B]) = {
      val e = f(r.getValue)
      new SimpleImmutableEntry(r.getKey ⊹ e.getKey, e.getValue)
    }
  }
  
  implicit def ValidationBind[X]: Bind[({type λ[α]=Validation[X, α]})#λ] = new Bind[({type λ[α]=Validation[X, α]})#λ] {
    def bind[A, B](r: Validation[X, A], f: A => Validation[X, B]) = r match {
      case Success(a) => f(a)
      case Failure(e) => Failure(e)
    }
  }

  implicit def ValidationFailureBind[X]: Bind[({type λ[α]=FailProjection[α, X]})#λ] = new Bind[({type λ[α]=FailProjection[α, X]})#λ] {
    def bind[A, B](r: FailProjection[A, X], f: A => FailProjection[B, X]) = r.validation match {
      case Success(a) => a.success.fail
      case Failure(e) => f(e)
    }
  }

  implicit def TreeBind: Bind[Tree] = new Bind[Tree] {
    def bind[A, B](t: Tree[A], f: A => Tree[B]): Tree[B] = {
      val r = f(t.rootLabel)
      node(r.rootLabel, r.subForest.append(t.subForest.map(bind(_, f))))
    }
  }

  import concurrent.Promise
  implicit def PromiseBind: Bind[Promise] = new Bind[Promise] {
    def bind[A, B](r: Promise[A], f: A => Promise[B]) = r bind f
  }
  
  implicit def IterVBind[E]: Bind[({type λ[α]=IterV[E, α]})#λ] = new Bind[({type λ[α]=IterV[E, α]})#λ] {
    import IterV._
    def bind[A, B](a: IterV[E, A], f: A => IterV[E, B]) = a.fold(
      done = (x, str) => f(x).fold(
        done = (x2, _) => Done(x2, str),
        cont = _(str)),
      cont = k => Cont(str2 => bind(k(str2), f)))
  }

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListBind: Bind[ArrayList] = new Bind[ArrayList] {
    def bind[A, B](r: ArrayList[A], f: A => ArrayList[B]) = {
      val a = new ArrayList[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaLinkedListBind: Bind[LinkedList] = new Bind[LinkedList] {
    def bind[A, B](r: LinkedList[A], f: A => LinkedList[B]) = {
      val a = new LinkedList[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaPriorityQueueBind: Bind[PriorityQueue] = new Bind[PriorityQueue] {
    def bind[A, B](r: PriorityQueue[A], f: A => PriorityQueue[B]) = {
      val a = new PriorityQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaStackBind: Bind[Stack] = new Bind[Stack] {
    def bind[A, B](r: Stack[A], f: A => Stack[B]) = {
      val a = new Stack[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaVectorBind: Bind[Vector] = new Bind[Vector] {
    def bind[A, B](r: Vector[A], f: A => Vector[B]) = {
      val a = new Vector[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaArrayBlockingQueueBind: Bind[ArrayBlockingQueue] = new Bind[ArrayBlockingQueue] {
    def bind[A, B](r: ArrayBlockingQueue[A], f: A => ArrayBlockingQueue[B]) = {
      val a = new ArrayBlockingQueue[B](r.remainingCapacity)
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaConcurrentLinkedQueueBind: Bind[ConcurrentLinkedQueue] = new Bind[ConcurrentLinkedQueue] {
    def bind[A, B](r: ConcurrentLinkedQueue[A], f: A => ConcurrentLinkedQueue[B]) = {
      val a = new ConcurrentLinkedQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaCopyOnWriteArrayListBind: Bind[CopyOnWriteArrayList] = new Bind[CopyOnWriteArrayList] {
    def bind[A, B](r: CopyOnWriteArrayList[A], f: A => CopyOnWriteArrayList[B]) = {
      val a = new CopyOnWriteArrayList[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaLinkedBlockingQueueBind: Bind[LinkedBlockingQueue] = new Bind[LinkedBlockingQueue] {
    def bind[A, B](r: LinkedBlockingQueue[A], f: A => LinkedBlockingQueue[B]) = {
      val a = new LinkedBlockingQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit def JavaSynchronousQueueBind: Bind[SynchronousQueue] = new Bind[SynchronousQueue] {
    def bind[A, B](r: SynchronousQueue[A], f: A => SynchronousQueue[B]) = {
      val a = new SynchronousQueue[B]
      val i = r.iterator
      while (i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }
}
