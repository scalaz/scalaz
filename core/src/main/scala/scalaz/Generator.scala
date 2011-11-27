package scalaz

/**
 * A `Generator[C]` is a container of elements, and which knows how to efficiently apply a [[scalaz.Reducer]]
 * to extract an answer by combining elements. A `Reducer` may supply efficient left-to-right and
 * right-to-left reduction strategies that a `Generator` may avail itself of.
 */
trait Generator[C[_]] {
  def reduce[E, M](r: Reducer[E, M], c: C[E]): M = to(r, r.zero, c)

  def to[E, M](r: Reducer[E, M], m: M, c: C[E]): M = r.append(m, reduce(r, c))

  def from[E, M](r: Reducer[E, M], c: C[E], m: M): M = r.append(reduce(r, c), m)
}

object Generator extends Generators

trait Generators {

  def FoldrGenerator[F[_] : Foldable]: Generator[F] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M =
      Foldable[F].foldR(c, r.zero)(a => b => r.cons(a, b))
  }

  def FoldlGenerator[F[_] : Foldable]: Generator[F] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M =
      Foldable[F].foldLeft(c, r.zero)((b, a) => r.snoc(b, a))
  }
}
