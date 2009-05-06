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
  import MA.ma
  def applicativeZip[F[_]](implicit f: Applicative[F]) = new Zip[F] {
    def zip[A, B](a: F[A], b: F[B]) = ma[F](a).liftA(b, (Tuple2(_: A, _: B)).curry)
  }

  def zipApply[Z[_]](implicit f: Functor[Z], z: Zip[Z]) = new Apply[Z] {
    def apply[A, B](fs: Z[A => B], a: Z[A]): Z[B] = z.zipWith((_: (A => B)).apply(_: A), fs, a)
  }

  implicit val IdentityZip: Zip[Identity] = new Zip[Identity] {
    def zip[A, B](a: Identity[A], b: Identity[B]): Identity[(A, B)] = (a.value, b.value)
  }

  implicit def ContinuationZip[R] = applicativeZip[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListZip = applicativeZip[NonEmptyList]

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

  implicit val ListZip = applicativeZip[List]

  implicit val StreamZip = applicativeZip[Stream]

  implicit val OptionZip = applicativeZip[Option]

  implicit val ArrayZip = applicativeZip[Array]

  implicit def EitherLeftZip[X] = applicativeZip[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightZip[X] = applicativeZip[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit def ValidationZip[X](implicit s: Semigroup[X]) = applicativeZip[PartialApply1Of2[Validation, X]#Apply]

  implicit def ValidationFailureZip[X] = applicativeZip[PartialApply1Of2[Validation.FailureProjection, X]#Flip]

  implicit val ZipperZip = applicativeZip[Zipper]

  implicit val TreeZip = applicativeZip[Tree]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListZip = applicativeZip[ArrayList]

  implicit val JavaLinkedListZip = applicativeZip[LinkedList]

  implicit val JavaPriorityQueueZip = applicativeZip[PriorityQueue]

  implicit val JavaStackZip = applicativeZip[Stack]

  implicit val JavaVectorZip = applicativeZip[Vector]

  implicit val JavaArrayBlockingQueueZip = applicativeZip[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueZip = applicativeZip[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListZip = applicativeZip[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueZip = applicativeZip[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueZip = applicativeZip[SynchronousQueue]
}
