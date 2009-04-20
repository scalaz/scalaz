package scalaz

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a stream. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */

sealed trait Zipper[+A] extends Iterable[A] {
  val a: A
  val ls: Stream[A]
  val rs: Stream[A]

  def elements = (ls.reverse ++ Stream.cons(a, rs)).elements
}

object Zipper {
  def zipper[A](lefts: Stream[A], focus: A, rights: Stream[A]) = new Zipper[A] {
    val a = focus;
    val ls = lefts;
    val rs = rights;
  }

  def zipper[A](focus: A) = new Zipper[A] {
    val a = focus;
    val ls = Stream.empty
    val rs = Stream.empty
  }

  def fromStream[A](s: Stream[A]) = new Zipper[A] {
    val a = s.head
    val ls = Stream.empty
    val rs = s.tail 
  }

  import S._
  import Show._
  
  implicit def ZipperShow[A](implicit sa: Show[A]): Show[Zipper[A]] = show((z: Zipper[A]) =>
    z.ls.reverse.show ++ " " ++ sa.show(z.a) ++ " " ++ z.rs.show)
}