package scalaz

trait Zip[F[_]] {
  def zip[A, B](a: F[A], b: F[B]): F[(A, B)]

  import S._
  import MA._
  def zipWith[A, B, C](h: (A, B) => C, a: F[A], b: F[B])(implicit f: Functor[F]): F[C] = {
    def map[M[_], X, Y](implicit t: Functor[M]) = (g: X => Y) => (m: M[X]) => ma[M](m).map(g)
    (map[PartialApply1Of2[Function1, F[A]]#Apply, (F[B] => F[(A, B)]), (F[B] => F[C])]
        compose map[PartialApply1Of2[Function1, F[B]]#Apply, F[(A, B)], F[C]]
        compose map[F, (A, B), C])(h.tupled)((zip(_: F[A], _: F[B])).curry)(a)(b)
  }

  def zipWith[A, B, C, D](h: (A, B, C) => D, as: F[A], bs: F[B], cs: F[C])(implicit f: Functor[F]): F[D] =
    zipWith((ab: (A, B), c: C) => h(ab._1, ab._2, c), zip(as, bs), cs)

}

object Zip {
  import S._
  import MA._
  import Copure._
  import Traverse._
  def applicativeZip[F[_]](implicit f: Applicative[F]) = new Zip[F] {
    def zip[A, B](a: F[A], b: F[B]) = ma[F](a).liftA(b, (Tuple2(_: A, _: B)).curry)
  }

  def zipApply[Z[_]](implicit f: Functor[Z], z: Zip[Z]) = new Apply[Z] {
    def apply[A, B](fs: Z[A => B], a: Z[A]): Z[B] = z.zipWith((_: (A => B)).apply(_: A), fs, a)
  }

  implicit val ZipStreamZip: Zip[ZipStream] = applicativeZip[ZipStream]

  implicit val IdentityZip: Zip[Identity] = applicativeZip[Identity]

  implicit def ContinuationZip[R] = applicativeZip[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListZip: Zip[NonEmptyList] = new Zip[NonEmptyList] {
    def zip[A, B](a: NonEmptyList[A], b: NonEmptyList[B]) =
      NonEmptyList.nel((a.head, b.head), ListZip.zip(a.tail, b.tail))
  }

  implicit def StateZip[S] = applicativeZip[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Zip = applicativeZip[Tuple1]

  implicit def Tuple2Zip[R](implicit sr: Monoid[R]) = applicativeZip[PartialApply1Of2[Tuple2, R]#Apply]

  implicit def Tuple3Zip[R, S](implicit sr: Monoid[R], ss: Monoid[S]) = applicativeZip[PartialApply2Of3[Tuple3, R, S]#Apply]

  implicit def Tuple4Zip[R, S, T](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T]) = applicativeZip[PartialApply3Of4[Tuple4, R, S, T]#Apply]

  implicit def Tuple5Zip[R, S, T, U](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U]) = applicativeZip[PartialApply4Of5[Tuple5, R, S, T, U]#Apply]

  implicit def Tuple6Zip[R, S, T, U, V](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U], sv: Monoid[V]) = applicativeZip[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply]

  implicit def Tuple7Zip[R, S, T, U, V, W](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U], sv: Monoid[V], sw: Monoid[W]) = applicativeZip[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply]

  implicit val Function0Zip = applicativeZip[Function0]

  implicit def Function1Zip[R] = applicativeZip[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Zip[R, S] = applicativeZip[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Zip[R, S, T] = applicativeZip[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Zip[R, S, T, U] = applicativeZip[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Zip[R, S, T, U, V] = applicativeZip[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Zip[R, S, T, U, V, W] = applicativeZip[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListZip: Zip[List] = new Zip[List] {
    def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
  }

  implicit val StreamZip: Zip[Stream] = new Zip[Stream] {
    def zip[A, B](a: Stream[A], b: Stream[B]): Stream[(A, B)] = a.zip(b)
  }

  implicit val OptionZip = applicativeZip[Option]

  implicit val ArrayZip = new Zip[Array] {
    def zip[A, B](a: Array[A], b: Array[B]): Array[(A, B)] = a.zip(b)
  }

  implicit def EitherLeftZip[X] = applicativeZip[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightZip[X] = applicativeZip[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit def ValidationZip[X](implicit s: Semigroup[X]) = applicativeZip[PartialApply1Of2[Validation, X]#Apply]

  implicit def ValidationFailureZip[X] = applicativeZip[PartialApply1Of2[Validation.FailureProjection, X]#Flip]

  implicit val ZipperZip: Zip[Zipper] = new Zip[Zipper] {
    def zip[A, B](a: Zipper[A], b: Zipper[B]): Zipper[(A, B)] =
      Zipper.zipper(StreamZip.zip(a.lefts, b.lefts), (a.focus, b.focus), StreamZip.zip(a.lefts, b.lefts))
  }

  implicit val TreeZip: Zip[Tree] = new Zip[Tree] {
    def zip[A, B](a: Tree[A], b: Tree[B]): Tree[(A, B)] =
      Tree.node((a.rootLabel, b.rootLabel),
        StreamZip.zip(a.subForest, b.subForest).map((TreeZip.zip(_: Tree[A], _: Tree[B])).tupled))
  }

  implicit val TreeLocZip: Zip[TreeLoc] = new Zip[TreeLoc] {
    def zip[A, B](a: TreeLoc[A], b: TreeLoc[B]): TreeLoc[(A, B)] = {
      val tzip = (TreeZip.zip(_: Tree[A], _: Tree[B])).tupled;
      TreeLoc.loc(TreeZip.zip(a.tree, b.tree),
        a.lefts.zip(b.lefts).map(tzip),
        a.rights.zip(b.rights).map(tzip),
        a.parents.zip(b.parents)
            .map((p: ((Stream[Tree[A]], A, Stream[Tree[A]]), (Stream[Tree[B]], B, Stream[Tree[B]]))) => p match {
          case ((la, a, ra), (lb, b, rb)) => (la.zip(lb).map(tzip), (a, b), ra.zip(rb).map(tzip))
        }))
    }
  }

  import java.util._
  import java.util.concurrent._
  import scala.Math._

  implicit val JavaArrayListZip: Zip[ArrayList] = new Zip[ArrayList] {
    def zip[A, B](a: ArrayList[A], b: ArrayList[B]): ArrayList[(A, B)] = {
      val s = min(a.size, b.size)
      val z: ArrayList[(A, B)] = new ArrayList[(A, B)](s)
      for (n <- 0 until s)
        z.add((a.get(n), b.get(n)))
      z
    }
  }

  implicit val JavaLinkedListZip: Zip[LinkedList] = new Zip[LinkedList] {
    def zip[A, B](a: LinkedList[A], b: LinkedList[B]): LinkedList[(A, B)] = {
      val z: LinkedList[(A, B)] = new LinkedList[(A, B)]
      val as = a.iterator
      val bs = b.iterator
      while (as.hasNext && bs.hasNext)
        z.add((as.next, bs.next))
      z
    }
  }

  implicit val JavaPriorityQueueZip: Zip[PriorityQueue] = new Zip[PriorityQueue] {
    def zip[A, B](a: PriorityQueue[A], b: PriorityQueue[B]): PriorityQueue[(A, B)] = {
      val z: PriorityQueue[(A, B)] = new PriorityQueue[(A, B)]
      val as = a.iterator
      val bs = b.iterator
      while (as.hasNext && bs.hasNext)
        z.add((as.next, bs.next))
      z
    }
  }

  implicit val JavaStackZip: Zip[Stack] = new Zip[Stack] {
    def zip[A, B](a: Stack[A], b: Stack[B]): Stack[(A, B)] = {
      val z: Stack[(A, B)] = new Stack[(A, B)]
      val s = min(a.size, b.size)
      for (n <- 0 until s)
        z.add((a.get(n), b.get(n)))
      z
    }
  }

  implicit val JavaVectorZip: Zip[Vector] = new Zip[Vector] {
    def zip[A, B](a: Vector[A], b: Vector[B]): Vector[(A, B)] = {
      val z: Vector[(A, B)] = new Vector[(A, B)](min(a.size, b.size))
      val s = min(a.size, b.size)
      for (n <- 0 until s)
        z.add((a.get(n), b.get(n)))
      z
    }
  }

  implicit val JavaArrayBlockingQueueZip: Zip[ArrayBlockingQueue] = new Zip[ArrayBlockingQueue] {
    def zip[A, B](a: ArrayBlockingQueue[A], b: ArrayBlockingQueue[B]): ArrayBlockingQueue[(A, B)] = {
      val z: ArrayBlockingQueue[(A, B)] = new ArrayBlockingQueue[(A, B)](min(a.size, b.size))
      val as = a.iterator
      val bs = b.iterator
      while (as.hasNext && bs.hasNext)
        z.add((as.next, bs.next))
      z
    }
  }

  implicit val JavaConcurrentLinkedQueueZip: Zip[ConcurrentLinkedQueue] = new Zip[ConcurrentLinkedQueue] {
    def zip[A, B](a: ConcurrentLinkedQueue[A], b: ConcurrentLinkedQueue[B]): ConcurrentLinkedQueue[(A, B)] = {
      val z: ConcurrentLinkedQueue[(A, B)] = new ConcurrentLinkedQueue[(A, B)]
      val as = a.iterator
      val bs = b.iterator
      while (as.hasNext && bs.hasNext)
        z.add((as.next, bs.next))
      z
    }
  }

  implicit val JavaCopyOnWriteArrayListZip: Zip[CopyOnWriteArrayList] = new Zip[CopyOnWriteArrayList] {
    def zip[A, B](a: CopyOnWriteArrayList[A], b: CopyOnWriteArrayList[B]): CopyOnWriteArrayList[(A, B)] = {
      val s = min(a.size, b.size)
      val z: CopyOnWriteArrayList[(A, B)] = new CopyOnWriteArrayList[(A, B)]
      for (n <- 0 until s)
        z.add((a.get(n), b.get(n)))
      z
    }
  }

  implicit val JavaLinkedBlockingQueueZip: Zip[LinkedBlockingQueue] = new Zip[LinkedBlockingQueue] {
    def zip[A, B](a: LinkedBlockingQueue[A], b: LinkedBlockingQueue[B]): LinkedBlockingQueue[(A, B)] = {
      val z: LinkedBlockingQueue[(A, B)] = new LinkedBlockingQueue[(A, B)]
      val as = a.iterator
      val bs = b.iterator
      while (as.hasNext && bs.hasNext)
        z.add((as.next, bs.next))
      z
    }
  }

  implicit val JavaSynchronousQueueZip: Zip[SynchronousQueue] = new Zip[SynchronousQueue] {
    def zip[A, B](a: SynchronousQueue[A], b: SynchronousQueue[B]): SynchronousQueue[(A, B)] = {
      val z: SynchronousQueue[(A, B)] = new SynchronousQueue[(A, B)]
      val as = a.iterator
      val bs = b.iterator
      while (as.hasNext && bs.hasNext)
        z.add((as.next, bs.next))
      z
    }
  }

}
