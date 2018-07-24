package scalaz

/**
 * ListT monad transformer.
 */

final case class ListT[M[_], A](run: M[IList[A]]){
  def uncons(implicit M: Applicative[M]): M[Option[(A, ListT[M, A])]] = {
    M.map(run) {
      case INil() => None
      case ICons(listHead, listTail) => Some((listHead, new ListT(M.point(listTail))))
    }
  }

  def ::(a: A)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(list => a :: list))

  def collect[B](pf: PartialFunction[A,B])(implicit M: Functor[M]): ListT[M, B] = new ListT(M.map(run)(_.collect(pf)))

  def isEmpty(implicit M: Functor[M]) : M[Boolean] = M.map(run)(_.isEmpty)

  def headOption(implicit M: Functor[M]) : OptionT[M, A] = new OptionT(M.map(run)(_.headOption))

  def find(predicate: A => Boolean)(implicit M: Functor[M]) : OptionT[M, A] = new OptionT(M.map(run)(_.find(predicate)))

  def headMaybe(implicit M: Functor[M]) : MaybeT[M, A] = new MaybeT(M.map(run)(l => Maybe.fromOption(l.headOption)))

  def tailM(implicit M: Applicative[M]) : M[ListT[M, A]] = M.map(uncons)(_.get._2)

  def filter(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = new ListT(M.map(run)(_.filter(p)))

  def drop(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.dropWhile(p)))

  def take(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.takeWhile(p)))

  def ++(bs: => ListT[M, A])(implicit M: Bind[M]) : ListT[M, A] = new ListT(M.bind(run){list1 =>
    M.map(bs.run){list2 =>
      list1 ++ list2
    }
  })

  def flatMap[B](f: A => ListT[M, B])(implicit M: Monad[M]) : ListT[M, B] =
    new ListT(M.bind(run)(Foldable[IList].foldMap(_)(f).run))

  def flatMapF[B](f: A => M[IList[B]])(implicit M: Monad[M]) : ListT[M, B] = flatMap(f andThen ListT.apply)

  def map[B](f: A => B)(implicit M: Functor[M]): ListT[M, B] = new ListT(
    M.map(run)(_.map(f))
  )

  def mapF[B](f: A => M[B])(implicit M: Monad[M]): ListT[M, B] = {
    flatMapF {
      f andThen (mb => M.map(mb)(b => IList(b)))
    }
  }

  def mapT[F[_], B](f: M[IList[A]] => F[IList[B]]): ListT[F, B] =
    ListT(f(run))

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.tailOption.get))

  def tailOption(implicit M: Functor[M]) : ListT[Lambda[a => M[Option[a]]], A] = new ListT[Lambda[a => M[Option[a]]], A](M.map(run)(_.tailOption))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldLeft(z){(left, right) => f(left, right)})

  def toIList : M[IList[A]] = run

  def toList(implicit M: Functor[M]): M[List[A]] = M.map(run)(_.toList)

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldRight(z){(right, left) => f(right, left)})

  def length(implicit M: Functor[M]) : M[Int] = M.map(run)(_.length)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class ListTInstances2 {
  implicit def listTFunctor[F[_]](implicit F0: Functor[F]): Functor[ListT[F, ?]] =
    new ListTFunctor[F]{
      implicit def F: Functor[F] = F0
    }

  implicit def listTSemigroup[F[_], A](implicit F0: Bind[F]): Semigroup[ListT[F, A]] =
    new ListTSemigroup[F, A]{
      implicit def F: Bind[F] = F0
    }
}

sealed abstract class ListTInstances1 extends ListTInstances2 {
  implicit def listTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[ListT[F, A]] =
    new ListTMonoid[F, A] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class ListTInstances extends ListTInstances1 {
  implicit def listTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[ListT[F, ?]] =
    new ListTMonadPlus[F] {
      implicit def F: Monad[F] = F0
    }

  implicit def listTEqual[F[_], A](implicit E: Equal[F[IList[A]]]): Equal[ListT[F, A]] =
    E.contramap((_: ListT[F, A]).toIList)

  implicit def listTShow[F[_], A](implicit E: Show[F[IList[A]]]): Show[ListT[F, A]] =
    Contravariant[Show].contramap(E)((_: ListT[F, A]).toIList)

  implicit val listTHoist: Hoist[ListT] =
    new ListTHoist {}
}

object ListT extends ListTInstances {
  def listT[M[_]]: (λ[α => M[IList[α]]] ~> ListT[M, ?]) =
    λ[λ[α => M[IList[α]]] ~> ListT[M, ?]](
      new ListT(_)
    )

  def empty[M[_], A](implicit M: Applicative[M]): ListT[M, A] =
    new ListT[M, A](M.point(INil()))

  def fromIList[M[_], A](mas: M[IList[A]]): ListT[M, A] =
    new ListT(mas)

  def fromList[M[_], A](mas: M[List[A]])(implicit M: Functor[M]): ListT[M, A] =
    new ListT(M.map(mas)(IList.fromList))
}

//
// Implementation traits for type class instances
//

private trait ListTDecidable[F[_]] extends Decidable[ListT[F, ?]] {
  implicit def F: Divisible[F]
  override def conquer[A]: ListT[F, A] = ListT(F.conquer)

  override def divide2[A1, A2, Z](a1: => ListT[F, A1], a2: => ListT[F, A2])(f: Z => (A1, A2)): ListT[F, Z] =
    ListT(F.divide2(a1.run, a2.run)((z: IList[Z]) => Unzip[IList].unzip(z.map(f))))

  override def choose2[Z, A1, A2](a1: => ListT[F, A1], a2: => ListT[F, A2])(f: Z => ∨[A1, A2]): ListT[F, Z] =
    ListT(
      F.divide2(a1.run, a2.run) (
        (z: IList[Z]) => z.map(f)
          ./:((IList.empty[A1], IList.empty[A2])) {
            case (x, y) => y.fold(a1 => (x._1 :+ a1, x._2), a2 => (x._1, x._2 :+ a2))
          }
      )
    )
}

private trait ListTFunctor[F[_]] extends Functor[ListT[F, ?]] {
 implicit def F: Functor[F]
 override def map[A, B](fa: ListT[F, A])(f: A => B): ListT[F, B] = fa map f
}

private trait ListTSemigroup[F[_], A] extends Semigroup[ListT[F, A]] {
 implicit def F: Bind[F]
 def append(f1: ListT[F, A], f2: => ListT[F, A]): ListT[F, A] = f1 ++ f2
}

private trait ListTMonoid[F[_], A] extends Monoid[ListT[F, A]] with ListTSemigroup[F, A] {
  implicit def F: Monad[F]

  def zero: ListT[F, A] = ListT.empty[F, A]
}

private trait ListTMonadPlus[F[_]] extends MonadPlus[ListT[F, ?]] with ListTFunctor[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: ListT[F, A])(f: A => ListT[F, B]): ListT[F, B] = fa flatMap f

  def point[A](a: => A): ListT[F, A] = a :: ListT.empty[F, A]

  def empty[A]: ListT[F, A] = ListT.empty[F, A]

  def plus[A](a: ListT[F, A], b: => ListT[F, A]): ListT[F, A] = a ++ b
}

private trait ListTHoist extends Hoist[ListT] {
  import ListT._

  implicit def apply[G[_] : Monad]: Monad[ListT[G, ?]] =
    listTMonadPlus[G]

  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): ListT[G, A] =
    fromIList(G.map(a)(entry => entry :: INil()))

  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): ListT[M, ?] ~> ListT[N, ?] =
    λ[ListT[M, ?] ~> ListT[N, ?]](_ mapT f.apply)
}
