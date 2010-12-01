package scalaz

import collection.generic.GenericTraversableTemplate
import collection.TraversableLike

trait Pure[P[_]] {
  def pure[A](a: => A): P[A]
}

object Pure {
  import Scalaz._

  implicit def IdentityPure: Pure[Identity] = new Pure[Identity] {
    def pure[A](a: => A) = a
  }

  implicit def ConstPure[B: Monoid] = new Pure[({type λ[α]=Const[B, α]})#λ] {
    def pure[A](a: => A) = Const[B, A](∅)
  }

  implicit def NonEmptyListPure: Pure[NonEmptyList] = new Pure[NonEmptyList] {
    def pure[A](a: => A) = a.wrapNel
  }

  implicit def TraversablePure[CC[X] <: TraversableLike[X, CC[X]] : CanBuildAnySelf]: Pure[CC] = new Pure[CC] {
    def pure[A](a: => A) = {
      val builder = implicitly[CanBuildAnySelf[CC]].apply[Nothing, A]
      builder += a
      builder.result
    }
  }

  implicit def StatePure[S]: Pure[({type λ[α]=State[S, α]})#λ] = new Pure[({type λ[α]=State[S, α]})#λ] {
    def pure[A](a: => A) = a.state[S]
  }

  implicit def StateTPure[S, M[_]: Pure]: Pure[({type λ[α]=StateT[M, S, α]})#λ] =
    new Pure[({type λ[α]=StateT[M, S, α]})#λ] {
      def pure[A](a: => A) = stateT(s => ((s, a)).pure[M])
    }

  implicit def Tuple1Pure = new Pure[Tuple1] {
    def pure[A](a: => A) = Tuple1(a)
  }

  implicit def Tuple2Pure[R: Zero]: Pure[({type λ[α]=(R, α)})#λ] = new Pure[({type λ[α]=(R, α)})#λ] {
    def pure[A](a: => A) = (∅, a)
  }

  implicit def Tuple3Pure[R: Zero, S: Zero]: Pure[({type λ[α]=(R, S, α)})#λ] = new Pure[({type λ[α]=(R, S, α)})#λ] {
    def pure[A](a: => A) = (∅[R], ∅[S], a)
  }

  implicit def Tuple4Pure[R: Zero, S: Zero, T: Zero]: Pure[({type λ[α]=(R, S, T, α)})#λ] = new Pure[({type λ[α]=(R, S, T, α)})#λ] {
    def pure[A](a: => A) = (∅[R], ∅[S], ∅[T], a)
  }

  implicit def Tuple5Pure[R: Zero, S: Zero, T: Zero, U: Zero]: Pure[({type λ[α]=(R, S, T, U, α)})#λ] = new Pure[({type λ[α]=(R, S, T, U, α)})#λ] {
    def pure[A](a: => A) = (∅[R], ∅[S], ∅[T], ∅[U], a)
  }

  implicit def Tuple6Pure[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero]: Pure[({type λ[α]=(R, S, T, U, V, α)})#λ] = new Pure[({type λ[α]=(R, S, T, U, V, α)})#λ] {
    def pure[A](a: => A) = (∅[R], ∅[S], ∅[T], ∅[U], ∅[V], a)
  }

  implicit def Tuple7Pure[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero, W: Zero]: Pure[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = new Pure[({type λ[α]=(R, S, T, U, V, W, α)})#λ] {
    def pure[A](a: => A) = (∅[R], ∅[S], ∅[T], ∅[U], ∅[V], ∅[W], a)
  }

  implicit def Function0Pure: Pure[Function0] = new Pure[Function0] {
    def pure[A](a: => A) = new Function0[A] {
      def apply = a
    }
  }

  implicit def Function1Pure[R]: Pure[({type λ[α]=(R) => α})#λ] = new Pure[({type λ[α]=(R) => α})#λ] {
    def pure[A](a: => A) = (_: R) => a
  }

  implicit def Function2Pure[R, S]: Pure[({type λ[α]=(R, S) => α})#λ] = new Pure[({type λ[α]=(R, S) => α})#λ] {
    def pure[A](a: => A) = (_: R, _: S) => a
  }

  implicit def Function3Pure[R, S, T]: Pure[({type λ[α]=(R, S, T) => α})#λ] = new Pure[({type λ[α]=(R, S, T) => α})#λ] {
    def pure[A](a: => A) = (_: R, _: S, _: T) => a
  }

  implicit def Function4Pure[R, S, T, U]: Pure[({type λ[α]=(R, S, T, U) => α})#λ] = new Pure[({type λ[α]=(R, S, T, U) => α})#λ] {
    def pure[A](a: => A) = (_: R, _: S, _: T, _: U) => a
  }

  implicit def Function5Pure[R, S, T, U, V]: Pure[({type λ[α]=(R, S, T, U, V) => α})#λ] = new Pure[({type λ[α]=(R, S, T, U, V) => α})#λ] {
    def pure[A](a: => A) = (_: R, _: S, _: T, _: U, _: V) => a
  }

  implicit def Function6Pure[R, S, T, U, V, W]: Pure[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = new Pure[({type λ[α]=(R, S, T, U, V, W) => α})#λ] {
    def pure[A](a: => A) = (_: R, _: S, _: T, _: U, _: V, _: W) => a
  }

  implicit def OptionPure: Pure[Option] = new Pure[Option] {
    def pure[A](a: => A) = Some(a)
  }

  implicit def FirstOptionPure: Pure[FirstOption] = new Pure[FirstOption] {
    def pure[A](a: => A) = Some(a).fst
  }
  
  implicit def LastOptionPure: Pure[LastOption] = new Pure[LastOption] {
    def pure[A](a: => A) = Some(a).lst
  }

  implicit def EitherLeftPure[X]: Pure[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Pure[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def pure[A](a: => A) = Left(a).left
  }

  implicit def EitherRightPure[X]: Pure[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Pure[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def pure[A](a: => A) = Right(a).right
  }

  implicit def ResponderPure: Pure[Responder] = new Pure[Responder] {
    def pure[A](a: => A) = new Responder[A] {
      def respond(k: A => Unit) = k(a)
    }
  }

  import java.util.concurrent.Callable

  implicit def CallablePure: Pure[Callable] = new Pure[Callable] {
    def pure[A](a: => A) = new Callable[A] {
      def call = a
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryPure[X: Zero]: Pure[({type λ[α]=Entry[X, α]})#λ] = new Pure[({type λ[α]=Entry[X, α]})#λ] {
    def pure[A](a: => A) = new SimpleImmutableEntry(∅, a)
  }

  implicit def ValidationPure[X]: Pure[({type λ[α]=Validation[X, α]})#λ] = new Pure[({type λ[α]=Validation[X, α]})#λ] {
    def pure[A](a: => A) = a.success
  }

  implicit def ValidationFailurePure[X]: Pure[({type λ[α]=FailProjection[α, X]})#λ] = new Pure[({type λ[α]=FailProjection[α, X]})#λ] {
    def pure[A](a: => A) = Failure(a).fail
  }
  
  implicit def IterVPure[E] = new Pure[({type λ[α]=IterV[E, α]})#λ] {
    import IterV._
    def pure[A](a: => A) = Done(a, Empty[E])
  }

  implicit def ZipperPure = new Pure[Zipper] {
    def pure[A](a: => A) = a.zipper
  }

  implicit def ZipStreamPure: Pure[ZipStream] = new Pure[ZipStream] {
    def pure[A](a: => A) = zip(Stream(a))
  }

  implicit def EndoPure: Pure[Endo] = new Pure[Endo] {
    def pure[A](a: => A) = constantEndo(a)
  }

  implicit def TreePure: Pure[Tree] = new Pure[Tree] {
    def pure[A](a: => A) = leaf(a)
  }

  implicit def TreeLocPure: Pure[TreeLoc] = new Pure[TreeLoc] {
    def pure[A](a: => A) = TreePure.pure(a).loc
  }

  import concurrent._
  implicit def PromisePure(implicit s: Strategy): Pure[Promise] = new Pure[Promise] {
    def pure[A](a: => A) = promise(a)
  }

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListPure: Pure[ArrayList] = new Pure[ArrayList] {
    def pure[A](a: => A) = {
      val k = new ArrayList[A]
      k add a
      k
    }
  }

  implicit def JavaHashSetPure: Pure[HashSet] = new Pure[HashSet] {
    def pure[A](a: => A) = {
      val k = new HashSet[A]
      k add a
      k
    }
  }

  implicit def JavaLinkedHashSetPure: Pure[LinkedHashSet] = new Pure[LinkedHashSet] {
    def pure[A](a: => A) = {
      val k = new LinkedHashSet[A]
      k add a
      k
    }
  }

  implicit def JavaLinkedListPure: Pure[LinkedList] = new Pure[LinkedList] {
    def pure[A](a: => A) = {
      val k = new LinkedList[A]
      k add a
      k
    }
  }

  implicit def JavaPriorityQueuePure: Pure[PriorityQueue] = new Pure[PriorityQueue] {
    def pure[A](a: => A) = {
      val k = new PriorityQueue[A]
      k add a
      k
    }
  }

  implicit def JavaStackPure: Pure[Stack] = new Pure[Stack] {
    def pure[A](a: => A) = {
      val k = new Stack[A]
      k add a
      k
    }
  }

  implicit def JavaTreeSetPure: Pure[TreeSet] = new Pure[TreeSet] {
    def pure[A](a: => A) = {
      val k = new TreeSet[A]
      k add a
      k
    }
  }

  implicit def JavaVectorPure: Pure[Vector] = new Pure[Vector] {
    def pure[A](a: => A) = {
      val k = new Vector[A]
      k add a
      k
    }
  }

  implicit def JavaArrayBlockingQueuePure: Pure[ArrayBlockingQueue] = new Pure[ArrayBlockingQueue] {
    def pure[A](a: => A) = {
      val k = new ArrayBlockingQueue[A](0)
      k add a
      k
    }
  }

  implicit def JavaConcurrentLinkedQueuePure: Pure[ConcurrentLinkedQueue] = new Pure[ConcurrentLinkedQueue] {
    def pure[A](a: => A) = {
      val k = new ConcurrentLinkedQueue[A]
      k add a
      k
    }
  }

  implicit def JavaCopyOnWriteArrayListPure: Pure[CopyOnWriteArrayList] = new Pure[CopyOnWriteArrayList] {
    def pure[A](a: => A) = {
      val k = new CopyOnWriteArrayList[A]
      k add a
      k
    }
  }

  implicit def JavaCopyOnWriteArraySetPure: Pure[CopyOnWriteArraySet] = new Pure[CopyOnWriteArraySet] {
    def pure[A](a: => A) = {
      val k = new CopyOnWriteArraySet[A]
      k add a
      k
    }
  }

  implicit def JavaLinkedBlockingQueuePure: Pure[LinkedBlockingQueue] = new Pure[LinkedBlockingQueue] {
    def pure[A](a: => A) = {
      val k = new LinkedBlockingQueue[A]
      k add a
      k
    }
  }

  implicit def JavaSynchronousQueuePure: Pure[SynchronousQueue] = new Pure[SynchronousQueue] {
    def pure[A](a: => A) = {
      val k = new SynchronousQueue[A]
      k add a
      k
    }
  }
}
