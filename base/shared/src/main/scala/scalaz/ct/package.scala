package scalaz

package object ct {

  val Kleisli: KleisliModule = KleisliImpl
  type Kleisli[F[_], A, B] = Kleisli.Kleisli[F, A, B]
}

package ct {
  trait CtFunctions
    extends scala.AnyRef
      with ApplicativeFunctions
      with ApplyFunctions
      with BifunctorFunctions
      with BindFunctions
      with CategoryFunctions
      with ChoiceFunctions
      with CobindFunctions
      with ComonadFunctions
      with ComposeFunctions
      with ContravariantFunctions
      with FoldableFunctions
      with FunctorFunctions
      with InvariantFunctorFunctions
      with PhantomFunctions
      with ProfunctorFunctions
      with StrongFunctions
      with TraversableFunctions
}