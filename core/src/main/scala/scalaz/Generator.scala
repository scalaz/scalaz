package scalaz

/**A Generator[C] is a container of elements, and which knows how to efficiently apply a Reducer to extract an answer by combining elements. A Reducer may supply efficient left-to-right and right-to-left reduction strategies that a Generator may avail itself of. **/
trait Generator[C[_]] {
  def reduce[E, M](r: Reducer[E, M], c: C[E]): M = to(r, r.z, c)

  def to[E, M](r: Reducer[E, M], m: M, c: C[E]): M = r.append(m, reduce(r, c))

  def from[E, M](r: Reducer[E, M], c: C[E], m: M): M = r.append(reduce(r, c), m)
}

object Generator extends Generators

trait Generators {

  import *->*._

  def FoldrGenerator[F[_] : Foldr]: Generator[F] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M =
      c.foldr(r.z)(a => b => r.cons(a, b))
  }

  def FoldlGenerator[F[_] : Foldl]: Generator[F] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M =
      c.foldl(r.z)(b => a => r.snoc(b, a))
  }

  def FoldMapGenerator[F[_] : FoldMap]: Generator[F] = new Generator[F] {
    override def reduce[E, M](r: Reducer[E, M], c: F[E]): M = {
      implicit val mm = r.monoid
      val y = implicitly[FoldMap[F]].foldMap[E, M](e => r.unit(e))
      y(c)
    }
  }
}
