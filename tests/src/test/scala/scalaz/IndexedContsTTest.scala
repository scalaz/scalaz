package scalaz

object IndexedContsTTest extends SpecLite {

  object instances {
    def functorRight[W[_]: Functor, M[_], R, O] = Functor[IndexedContsT[W, M, R, O, ?]]
    def functorLeft[W[_], M[_]: Functor, O, A] = Functor[IndexedContsT[W, M, ?, O, A]]
    def contravariant[W[_]: Functor, M[_]: Functor, R, A] = Contravariant[IndexedContsT[W, M, R, ?, A]]
    def bifunctor[W[_]: Functor, M[_]: Functor, O] = Bifunctor[IndexedContsT[W, M, ?, O, ?]]
    def bind[W[_]: Cobind, M[_], R] = Bind[ContsT[W, M, R, ?]]
    def monad[W[_]: Comonad, M[_], R] = Monad[ContsT[W, M, R, ?]]

    // checking absence of ambiguity
    def functor[W[_]: Comonad, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def functor[W[_]: Cobind, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def bind[W[_]: Comonad, M[_], R] = Bind[ContsT[W, M, R, ?]]
  }
}
