package scalaz

object IndexedContsTTest extends SpecLite {

  object instances {
    def functorRight[W[_]: Functor, M[_], R, O] = Functor[({type l[a] = IndexedContsT[W, M, R, O, a]})#l]
    def functorLeft[W[_], M[_]: Functor, O, A] = Functor[({type l[a] = IndexedContsT[W, M, a, O, A]})#l]
    def contravariant[W[_]: Functor, M[_]: Functor, R, A] = Contravariant[({type l[a] = IndexedContsT[W, M, R, a, A]})#l]
    def bifunctor[W[_]: Functor, M[_]: Functor, O] = Bifunctor[({type l[a, b] = IndexedContsT[W, M, a, O, b]})#l]
    def monad[W[_]: Comonad, M[_], R] = Monad[({type l[a] = ContsT[W, M, R, a]})#l]

    // checking absence of ambiguity
    def functor[W[_]: Comonad, M[_], R] = Functor[({type l[a] = ContsT[W, M, R, a]})#l]
  }
}
