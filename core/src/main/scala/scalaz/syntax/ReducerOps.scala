package scalaz
package syntax


final class ReducerOps[A](private val self: A) extends AnyVal {
  /** Convert the value into a monoid */
  def unit[M](implicit r: Reducer[A,M]): M = r.unit(self)

  /** Append the value to a monoid for use in left-to-right reduction */
  def snoc[C](c: C)(implicit r: RightReducer[C,A]): A = r.snoc(self, c)

  /** Prepend the value to a monoid for use in right-to-left reduction */
  def cons[M](m: M)(implicit r: LeftReducer[A,M]): M = r.cons(self, m)

  import ReducerOps._

  /** Unfold to the left using this value as initial seed.
    *
    * Example:
    * {{{
    * 0.unfoldl(i => if (i < 5) just((i+1, i.toString)) else empty[(Int, String)]).to[List]
    *  = List(4, 3, 2, 1, 0)
    * }}}
    */
  def unfoldl[C](f: A => Maybe[(A, C)]): UnfoldlTo[A, C] = new UnfoldlTo(self, f)

  /** Unfold to the right using this value as initial seed
    * Example:
    * {{{
    * 0.unfoldr(i => if (i < 5) just((i.toString, i+1)) else empty[(String, Int)]).to[List]
    *  = List(0, 1, 2, 3, 4)
    * }}}
    */
  def unfoldr[C](f: A => Maybe[(C, A)]): UnfoldrTo[A, C] = new UnfoldrTo(self, f)
}

trait ToReducerOps {
  implicit def ToReducerOps[A](a: A): ReducerOps[A] = new ReducerOps(a)
}

object ReducerOps {

  final class UnfoldlTo[A, C](a: A, f: A => Maybe[(A, C)]) {
    def reduceToOpt[M](implicit r: LeftReducer[C, M]): Maybe[M] = r.unfoldlOpt(a)(f)
    def reduceTo[M: Monoid](implicit r: LeftReducer[C, M]): M = r.unfoldl(a)(f)

    def to[M[_]](implicit r: LeftReducer[C, M[C]], m: Monoid[M[C]]): M[C] = reduceTo[M[C]]
    def toOpt[M[_]](implicit r: LeftReducer[C, M[C]]): Maybe[M[C]] = reduceToOpt[M[C]]
  }

  final class UnfoldrTo[A, C](a: A, f: A => Maybe[(C, A)]) {
    def reduceToOpt[M](implicit r: RightReducer[C, M]): Maybe[M] = r.unfoldrOpt(a)(f)
    def reduceTo[M: Monoid](implicit r: RightReducer[C, M]): M = r.unfoldr(a)(f)

    def to[M[_]](implicit r: RightReducer[C, M[C]], m: Monoid[M[C]]): M[C] = reduceTo[M[C]]
    def toOpt[M[_]](implicit r: RightReducer[C, M[C]]): Maybe[M[C]] = reduceToOpt[M[C]]
  }
}
