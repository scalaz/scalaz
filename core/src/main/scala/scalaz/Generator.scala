package scalaz

/**
 * `Generator` a class of container of elements that knows how to efficiently apply a [[scalaz.Reducer]]
 * to extract an answer by combining elements. A `Reducer` may supply efficient left-to-right and
 * right-to-left reduction strategies that a `Generator` may avail itself of.
 */
trait Generator[C, Elem] {

  def mapReduce[E, M](e: Elem => E, c: C)(implicit r: Reducer[E, M], m: Monoid[M]): M = mapTo(e, m.zero, c)

  def mapTo[E, M](e: Elem => E, m: M, c: C)(implicit r: Reducer[E, M], M: Monoid[M]): M = r.append(m, mapReduce(e, c))

  def mapFrom[E, M](e: Elem => E, c: C, m: M)(implicit r: Reducer[E, M], M: Monoid[M]): M = r.append(mapReduce(e, c), m)
}

object Generator extends Generators

trait Generators {

  def FoldrGenerator[F[_] : Foldable, A]: Generator[F[A], A] = new Generator[F[A], A] {

    override def mapReduce[E, M](e: A => E, c: F[A])(implicit r: Reducer[E, M], m: Monoid[M]): M =
      Foldable[F].foldRight(c, m.zero)((a, b) => r.cons(e(a), b))
  }

  def FoldlGenerator[F[_] : Foldable, A]: Generator[F[A], A] = new Generator[F[A], A] {
    override def mapReduce[E, M](e: A => E, c: F[A])(implicit r: Reducer[E, M], m: Monoid[M]): M =
      Foldable[F].foldLeft(c, m.zero)((b, a) => r.snoc(b, e(a)))
  }

}
