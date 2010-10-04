package scalaz

import Scalaz._

/** A Generator[C] is a container of elements, and which knows how to efficiently apply a Reducer to extract an answer by combining elements. A Reducer may supply efficient left-to-right and right-to-left reduction strategies that a Generator may avail itself of. **/
abstract class Generator[C[_]] {
  def reduce[E, M](r: Reducer[E, M], c: C[E]): M = to(r, r.monoid.zero, c)
  def to[E, M](r: Reducer[E, M], m: M, c: C[E]): M = r.append(m, reduce(r, c))
  def from[E, M](r: Reducer[E, M], c: C[E], m: M): M = r.append(reduce(r, c), m)
}

object Generator {
  def FoldrGenerator[F[_]: Foldable] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M =
      c.foldr(r.monoid.zero)((a, b) => r.cons(a, b))
  }

  def FoldlGenerator[F[_]: Foldable] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M =
      c.foldl(r.monoid.zero)(r.snoc)
  }

  def FoldMapGenerator[F[_]: Foldable] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M = {
      c.foldMap(r.unit)(implicitly[Foldable[F]], r)
    }
  }
}
