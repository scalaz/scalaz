package scalaz

sealed trait NonEmptyList[A] {
  val head: A
  val tail: List[A]

  import NonEmptyList._
  import Zipper._

  def <::(b: A): NonEmptyList[A] = nel(b, head :: tail)

  import collection.mutable.ListBuffer

  def <:::(bs: List[A]): NonEmptyList[A] = {
    val b = new ListBuffer[A]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def :::>(bs: List[A]): NonEmptyList[A] = nel(head, tail ::: bs)

  def map[B](f: A => B): NonEmptyList[B] = nel(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = {
    val b = new ListBuffer[B]
    val p = f(head)
    b += p.head
    b ++= p.tail
    tail.foreach(a => {
      val p = f(a)
      b += p.head
      b ++= p.tail
    })
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def list: List[A] = head :: tail

  def stream: Stream[A] = head #:: tail.toStream

  def toZipper: Zipper[A] = zipper(Stream.Empty, head, tail.toStream)

  def zipperEnd: Zipper[A] = {
    import Stream._
    tail reverse match {
      case Nil => zipper(empty, head, empty)
      case t :: ts => zipper(ts.toStream :+ head, t, empty)
    }
  }

  def tails: NonEmptyList[NonEmptyList[A]] = nel(this, tail match {
    case Nil => Nil
    case h :: t => nel(h, t).tails.list
  })

  def reverse: NonEmptyList[A] = (list.reverse: @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  override def toString: String = "NonEmpty" + (head :: tail)
}

object NonEmptyList extends NonEmptyLists {
  def apply[A](h: A, t: A*): NonEmptyList[A] =
    nels(h, t: _*)
}

trait NonEmptyLists {
  def nel[A](h: A, t: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }

  def nels[A](h: A, t: A*): NonEmptyList[A] =
    nel(h, t.toList)

  implicit val NonEmptyListFoldl: Foldl[NonEmptyList] = new Foldl[NonEmptyList] {
    def foldl[A, B] = k => b => _.list.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit val NonEmptyListFoldr: Foldr[NonEmptyList] = new Foldr[NonEmptyList] {
    def foldr[A, B] = k => b => _.list.foldRight(b)((b, a) => k(b)(a))
  }

  implicit val NonEmptyListFoldable: Foldable[NonEmptyList] =
    Foldable.foldable[NonEmptyList]

  implicit val NonEmptyListFunctor: Functor[NonEmptyList] = new Functor[NonEmptyList] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit val NonEmptyListPointed: Pointed[NonEmptyList] = new Pointed[NonEmptyList] {
    def point[A](a: => A) =
      nels(a)
  }

  implicit val NonEmptyListApplic: Applic[NonEmptyList] = new Applic[NonEmptyList] {
    def applic[A, B](f: NonEmptyList[A => B]) =
      r =>
        for {
          ff <- f
          rr <- r
        } yield ff(rr)
  }

  implicit val NonEmptyListJoin: Join[NonEmptyList] = new Join[NonEmptyList] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit val NonEmptyListBind: Bind[NonEmptyList] = new Bind[NonEmptyList] {
    def bind[A, B](f: A => NonEmptyList[B]) =
      _ flatMap f
  }

  implicit val NonEmptyListPointedFunctor: PointedFunctor[NonEmptyList] =
    PointedFunctor.pointedFunctor

  implicit val NonEmptyListApplicative: Applicative[NonEmptyList] =
    Applicative.applicative

  implicit def NonEmptyListCojoin: CoJoin[NonEmptyList] = new CoJoin[NonEmptyList] {
    def coJoin[A] = _.tails
  }

  implicit val NonEmptyListApplicFunctor: ApplicFunctor[NonEmptyList] =
    ApplicFunctor.applicFunctor

  implicit val NonEmptyListBindFunctor: BindFunctor[NonEmptyList] =
    BindFunctor.bindFunctor

  implicit val NonEmptyListMonad: Monad[NonEmptyList] =
    Monad.monadBP

  implicit def NonEmptyListShow[A: Show]: Show[NonEmptyList[A]] =
    implicitly[Show[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

  implicit def NonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] =
    implicitly[Equal[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

  implicit def NonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] =
    implicitly[Order[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

  implicit def NonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] =
    Semigroup.semigroup(a1 => a1.list <::: _)

}
