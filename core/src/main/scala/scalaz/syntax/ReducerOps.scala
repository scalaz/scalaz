package scalaz
package syntax


final class ReducerOps[A](private val self: A) extends AnyVal {
  /** Convert the value into a monoid */
  def unit[M](implicit r: Reducer[A,M]): M = r.unit(self)

  /** Append the value to a monoid for use in left-to-right reduction */
  def snoc[C](c: C)(implicit r: Reducer[C,A]): A = r.snoc(self, c)

  /** Prepend the value to a monoid for use in right-to-left reduction */
  def cons[M](m: M)(implicit r: Reducer[A,M]): M = r.cons(self, m)

  import ReducerOps._

  /** Unfold to the left using this value as initial seed.
    *
    * Example:
    * {{{
    * 0.unfoldl(i => if (i < 5) just((i+1, i.toString)) else empty[(Int, String)]).to[List]
    *  = List(4, 3, 2, 1, 0)
    * }}}
    */
  def unfoldl[C](f: A => Maybe[(A, C)]): UnfoldTo[C] = new UnfoldTo[C] {
    def reduceTo[M](implicit r: Reducer[C, M]): M = r.unfoldl(self)(f)
  }

  /** Unfold to the right using this value as initial seed
    * Example:
    * {{{
    * 0.unfoldr(i => if (i < 5) just((i.toString, i+1)) else empty[(String, Int)]).to[List]
    *  = List(0, 1, 2, 3, 4)
    * }}}
    */
  def unfoldr[C](f: A => Maybe[(C, A)]): UnfoldTo[C] = new UnfoldTo[C] {
    def reduceTo[M](implicit r: Reducer[C, M]): M = r.unfoldr(self)(f)
  }
}

trait ToReducerOps {
  implicit def ToReducerOps[A](a: A): ReducerOps[A] = new ReducerOps(a)
}

object ReducerOps {

  sealed abstract class UnfoldTo[C] {
    def reduceTo[M](implicit r: Reducer[C, M]): M

    final def to[M[_]](implicit r: Reducer[C, M[C]]): M[C] = reduceTo[M[C]]
  }
}