package scalaz
package typeclass

trait MonadInstances {
  implicit val option: Monad[Option] = new MonadClass.Template[Option] {
    override def ap[A, B](oa: Option[A])(f: Option[A => B]): Option[B] = oa.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)
    override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)
    override def pure[A](a: A): Option[A] = Option(a)
    override def mapConst[A, B](ma: Option[A])(c: B): Option[B] = ???
    override def tapM[A, B](ma: Option[A])(f: (A) => Option[B]): Option[A] = ma match {
      case Some(s) => if (f(s).isDefined) ma else None
      case None => None
    }
  }

  implicit val list: Monad[List] = new MonadClass.Template[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B] = xs.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    override def pure[A](a: A): List[A] = List(a)
    override def mapConst[A, B](ma: List[A])(c: B): List[B] = ma.foldLeft[List[B]](Nil)((l, _) => c :: l)
    override def tapM[A, B](ma: List[A])(f: (A) => List[B]): List[A] =
      ma.foldLeft[List[A]](ma)((l, a) => mapConst(f(a))(a) )
  }
  
  implicit val function: Monad[Function0] = new MonadClass.Template[Function0] {
    override def ap[A, B](fab: Function0[A])(f: Function0[A => B]): Function0[B] = () => f()(fab())
    override def map[A, B](fab: Function0[A])(f: A => B): Function0[B] = () => f(fab())
    override def flatMap[A, B](fab: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(fab())()
    override def pure[A](a: A): Function0[A] = () => a
    override def mapConst[A, B](ma: () => A)(c: B): () => B = () => c
    override def tapM[A, B](ma: () => A)(f: (A) => () => B): () => A = ma
  }

  implicit def function1[C]: Monad[Function1[C, ?]] = new MonadClass.Template[Function1[C, ?]] {
    override def ap[A, B](fab: Function1[C, A])(f: Function1[C, A => B]): Function1[C, B] = (c: C) => f(c)(fab(c))
    override def map[A, B](fab: Function1[C, A])(f: A => B): Function1[C, B] = fab andThen f
    override def flatMap[A, B](fab: Function1[C, A])(f: A => Function1[C, B]): Function1[C, B] = (c: C) => f(fab(c))(c)
    override def pure[A](a: A): Function1[C, A] = (c: C) => a
    override def mapConst[A, B](ma: (C) => A)(b: B): (C) => B = (c: C) => b
    override def tapM[A, B](ma: (C) => A)(f: (A) => (C) => B): (C) => A = (c: C) => ma(c)
  }
}
