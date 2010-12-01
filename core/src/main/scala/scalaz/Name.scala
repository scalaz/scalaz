package scalaz

import collection.generic.GenericTraversableTemplate
import collection.TraversableLike

/** Call by name */
sealed abstract class Name[+A] {
  def value: A
  def >>=[B](f: A => B) : B = f(value)
  def flatMap[B](f: A => B) : B = f(value)
  def map[B](f: A => B): Name[B] = Name(f(value))
}

object Name { 
  def apply[A](a: => A) = new Name[A] {
    def value = a
  }
  def unapply[A](v: Name[A]): Option[A] = Some(v.value)

  implicit val nameMonad: Monad[Name] = new Monad[Name] {
    def bind[A,B](v: Name[A], f: A => Name[B]): Name[B] = 
      f(v.value)
    def pure[A](a: => A) = Name(a)
  }

}

trait Names {
  implicit def pureName[F[_],A](a: Name[A])(implicit p: Pure[F]): F[A] = p.pure(a.value)

  import Pure._
  import java.util.concurrent.Callable
  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  import java.util.{List => _, _} 
  import java.util.concurrent._
  import scalaz.concurrent._

  def nameToTraversable[A,CC[X] <: TraversableLike[X, CC[X]] : CanBuildAnySelf](a : Name[A]): CC[A] = pureName[CC,A](a)(TraversablePure[CC])

  implicit def nameToCallable[A] = pureName[Callable,A] _
  implicit def nameToConst[A,B: Monoid]: Name[A] => Const[B, A] = pureName[PartialApply1Of2[Const, B]#Apply,A] _
  implicit def nameToEitherLeft[A,X] = pureName[PartialApply1Of2[Either.LeftProjection, X]#Flip,A] _
  implicit def nameToEitherRight[A,X] = pureName[PartialApply1Of2[Either.RightProjection, X]#Apply,A] _
  implicit def nameToEndo[A] = pureName[Endo,A] _
  implicit def nameToEphemeralStream[A] = pureName[EphemeralStream, A] _
  implicit def nameToFirstOption[A] = pureName[FirstOption,A] _
  implicit def nameToFunction0[A] = pureName[Function0,A] _
  implicit def nameToFunction1[A,R] = pureName[PartialApply1Of2[Function1, R]#Apply,A] _
  implicit def nameToFunction2[A,R, S] = pureName[PartialApply2Of3[Function2, R, S]#Apply,A] _
  implicit def nameToFunction3[A,R, S, T] = pureName[PartialApply3Of4[Function3, R, S, T]#Apply,A] _
  implicit def nameToFunction4[A,R, S, T, U] = pureName[PartialApply4Of5[Function4, R, S, T, U]#Apply,A] _
  implicit def nameToFunction5[A,R, S, T, U, V] = pureName[PartialApply5Of6[Function5, R, S, T, U, V]#Apply,A] _
  implicit def nameToFunction6[A,R, S, T, U, V, W] = pureName[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply,A] _
  implicit def nameToIdentity[A] = pureName[Identity,A] _
  implicit def nameToIterV[A,E]  = pureName[PartialApply1Of2[IterV, E]#Apply,A] _
  implicit def nameToJavaArrayBlockingQueue[A] = pureName[ArrayBlockingQueue,A] _
  implicit def nameToJavaArrayList[A] = pureName[ArrayList,A] _
  implicit def nameToJavaConcurrentLinkedQueue[A] = pureName[ConcurrentLinkedQueue,A] _
  implicit def nameToJavaCopyOnWriteArrayList[A] = pureName[CopyOnWriteArrayList,A] _
  implicit def nameToJavaCopyOnWriteArraySet[A] = pureName[CopyOnWriteArraySet,A] _
  implicit def nameToJavaHashSet[A] = pureName[HashSet,A] _
  implicit def nameToJavaLinkedBlockingQueue[A] = pureName[LinkedBlockingQueue,A] _
  implicit def nameToJavaLinkedHashSet[A] = pureName[LinkedHashSet,A] _
  implicit def nameToJavaLinkedList[A] = pureName[LinkedList,A] _
  implicit def nameToJavaPriorityQueue[A] = pureName[PriorityQueue,A] _
  implicit def nameToJavaStack[A] = pureName[Stack,A] _
  implicit def nameToJavaSynchronousQueue[A] = pureName[SynchronousQueue,A] _  
  implicit def nameToJavaTreeSet[A] = pureName[TreeSet,A] _
  implicit def nameToJavaVector[A] = pureName[Vector,A] _
  implicit def nameToLastOption[A] = pureName[LastOption,A] _
  implicit def nameToList[A] = nameToTraversable[A, scala.List] _
  implicit def nameToMapEntry[A,X: Zero] = pureName[PartialApply1Of2[Entry, X]#Apply,A] _
  implicit def nameToNonEmptyList[A] = pureName[NonEmptyList,A] _
  implicit def nameToOption[A] = pureName[Option,A] _
  implicit def nameToPromise[A](implicit s: Strategy) = pureName[Promise,A] _
  implicit def nameToResponder[A] = pureName[Responder,A] _
  //implicit def nameToSet[A] = nameToTraversable[A, Set] _
  implicit def nameToStateT[M[_]:Pure, S, A](a: Name[A]): StateT[M, S, A] = pureName[PartialApplyKA[StateT, M, S]#Apply,A](a)
  implicit def nameToState[A,S] = pureName[PartialApply1Of2[State, S]#Apply,A] _
  implicit def nameToStream[A] = nameToTraversable[A, Stream] _
  implicit def nameToTreeLoc[A] = pureName[TreeLoc,A] _
  implicit def nameToTree[A] = pureName[Tree,A] _
  implicit def nameToTuple1[A]  = pureName[Tuple1,A] _
  implicit def nameToTuple2[A,R: Zero] = pureName[PartialApply1Of2[Tuple2, R]#Apply,A] _
  implicit def nameToTuple3[A,R: Zero, S: Zero] = pureName[PartialApply2Of3[Tuple3, R, S]#Apply,A] _
  implicit def nameToTuple4[A,R: Zero, S: Zero, T: Zero] = pureName[PartialApply3Of4[Tuple4, R, S, T]#Apply,A] _
  implicit def nameToTuple5[A,R: Zero, S: Zero, T: Zero, U: Zero] = pureName[PartialApply4Of5[Tuple5, R, S, T, U]#Apply,A] _
  implicit def nameToTuple6[A,R: Zero, S: Zero, T: Zero, U: Zero, V: Zero] = pureName[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply,A] _
  implicit def nameToTuple7[A,R: Zero, S: Zero, T: Zero, U: Zero, V: Zero, W: Zero] = pureName[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply,A] _
  implicit def nameToValidationFailure[A,X] = pureName[PartialApply1Of2[FailProjection, X]#Flip,A] _
  implicit def nameToValidation[A,X] = pureName[PartialApply1Of2[Validation, X]#Apply,A] _
  implicit def nameToZipStream[A] = pureName[ZipStream,A] _
  implicit def nameToZipper[A]: Name[A] => Zipper[A]  = pureName[Zipper,A] _
}

/** Call by need */
sealed abstract class Need[+A](a: => A) extends Name[A]
object Need {
  def apply[A](a: => A) = {
    lazy val value: A = a
    Name(value)
  }
  def unapply[A](x: Need[A]): Option[A] = Some(x.value)
}

/** Call by value */
case class Value[+A](value: A) extends Need[A](value)
