package scalaz

// compiles == green
object ImplicitConversionTest {
  import Scalaz._

  def MAs[A, B, C, D, E, F, G, H] {
    implicitly[List[A] <%< MA[List, A]]
    implicitly[Option[A] <%< MA[Option, A]]
    implicitly[(A => B) <%< MACofunctor[PartialApply1Of2[Function1, B]#Flip, A]]
    implicitly[(A => B) <%< MA[PartialApply1Of2[Function1, A]#Apply, B]]
    implicitly[((A, B) => C) <%< MA[PartialApply2Of3[Function2, A, B]#Apply, C]]
    implicitly[((A, B, C) => D) <%< MA[PartialApply3Of4[Function3, A, B, C]#Apply, D]]
    implicitly[((A, B, C, D) => E) <%< MA[PartialApply4Of5[Function4, A, B, C, D]#Apply, E]]
    implicitly[((A, B, C, D, E) => F) <%< MA[PartialApply5Of6[Function5, A, B, C, D, E]#Apply, F]]
    implicitly[((A, B, C, D, E, F) => G) <%< MA[PartialApply6Of7[Function6, A, B, C, D, E, F]#Apply, G]]
    implicitly[Validation[A, B] <%< MA[PartialApply1Of2[Validation, A]#Apply, B]]
    implicitly[FailProjection[A, B] <%< MA[PartialApply1Of2[FailProjection, B]#Flip, A]]
    implicitly[Either.LeftProjection[A, B] <%< MA[PartialApply1Of2[Either.LeftProjection, B]#Flip, A]]
    implicitly[Either.RightProjection[A, B] <%< MA[PartialApply1Of2[Either.RightProjection, A]#Apply, B]]
    implicitly[(A, B) <%< MA[PartialApply1Of2[Tuple2, A]#Apply, B]]
    implicitly[(A, B, C) <%< MA[PartialApply2Of3[Tuple3, A, B]#Apply, C]]
    implicitly[(A, B, C, D) <%< MA[PartialApply3Of4[Tuple4, A, B, C]#Apply, D]]
    implicitly[(A, B, C, D, E) <%< MA[PartialApply4Of5[Tuple5, A, B, C, D]#Apply, E]]
    implicitly[(A, B, C, D, E, F) <%< MA[PartialApply5Of6[Tuple6, A, B, C, D, E]#Apply, F]]
    implicitly[(A, B, C, D, E, F, G) <%< MA[PartialApply6Of7[Tuple7, A, B, C, D, E, F]#Apply, G]]

    // via higher kind inference
    trait T[A]
    implicitly[T[A] <%< MACofunctor[T, A]]
    implicitly[T[A] <%< MA[T, A]]
  }

  def apply[A, B, R, S, T, U, V, W, X] {
    implicitly[Apply[Identity]]
    implicitly[Apply[List]]
    implicitly[Apply[Function0]]
    implicitly[Apply[Option]]
    implicitly[Apply[PartialApply1Of2[State, A]#Apply]]
    implicitly[Apply[Function0]]
    implicitly[Apply[PartialApply1Of2[Function1, R]#Apply]]
    implicitly[Apply[PartialApply2Of3[Function2, R, S]#Apply]]
    implicitly[Apply[PartialApply3Of4[Function3, R, S, T]#Apply]]
    implicitly[Apply[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    implicitly[Apply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    implicitly[Apply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]]
    implicitly[Apply[PartialApply1Of2[Either.LeftProjection, X]#Flip]]
    implicitly[Apply[PartialApply1Of2[Either.RightProjection, X]#Apply]]
    import java.util.Map.Entry
    implicitly[Apply[PartialApply1Of2[Entry, Int]#Apply]]
    implicitly[Apply[PartialApply1Of2[Validation, Int]#Apply]]
    implicitly[Apply[PartialApply1Of2[FailProjection, X]#Flip]]
  }

  def applicative[A, B, R, S, T, U, V, W, X] {
    implicitly[Applicative[Identity]]
    implicitly[Applicative[List]]
    implicitly[Applicative[Function0]]
    implicitly[Applicative[Option]]
    implicitly[Applicative[PartialApply1Of2[State, A]#Apply]]
    implicitly[Applicative[Function0]]
    implicitly[Applicative[PartialApply1Of2[Function1, R]#Apply]]
    implicitly[Applicative[PartialApply2Of3[Function2, R, S]#Apply]]
    implicitly[Applicative[PartialApply3Of4[Function3, R, S, T]#Apply]]
    implicitly[Applicative[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    implicitly[Applicative[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    implicitly[Applicative[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]]
    implicitly[Applicative[PartialApply1Of2[Either.LeftProjection, X]#Flip]]
    implicitly[Applicative[PartialApply1Of2[Either.RightProjection, X]#Apply]]
    import java.util.Map.Entry
    implicitly[Applicative[PartialApply1Of2[Entry, Int]#Apply]]
    implicitly[Applicative[PartialApply1Of2[Validation, Int]#Apply]]
    implicitly[Applicative[PartialApply1Of2[FailProjection, X]#Flip]]
  }

  def pointed[A, B, R, S, T, U, V, W, X] {
    implicitly[Pointed[Identity]]
    implicitly[Pointed[List]]
    implicitly[Pointed[Function0]]
    implicitly[Pointed[Option]]
    implicitly[Pointed[PartialApply1Of2[State, A]#Apply]]
    implicitly[Pointed[Function0]]
    implicitly[Pointed[PartialApply1Of2[Function1, R]#Apply]]
    implicitly[Pointed[PartialApply2Of3[Function2, R, S]#Apply]]
    implicitly[Pointed[PartialApply3Of4[Function3, R, S, T]#Apply]]
    implicitly[Pointed[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    implicitly[Pointed[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    implicitly[Pointed[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]]
    implicitly[Pointed[PartialApply1Of2[Either.LeftProjection, X]#Flip]]
    implicitly[Pointed[PartialApply1Of2[Either.RightProjection, X]#Apply]]
    import java.util.Map.Entry
    implicitly[Pointed[PartialApply1Of2[Entry, Int]#Apply]]
    implicitly[Pointed[PartialApply1Of2[Validation, Int]#Apply]]
    implicitly[Pointed[PartialApply1Of2[FailProjection, X]#Flip]]
  }

  def bind {
    implicitly[Bind[List]]
    implicitly[Bind[ArraySeq]]
    implicitly[Bind[java.util.ArrayList]]
  }

  def monoid {
    implicitly[Monoid[List[Int]]]
    implicitly[Monoid[Iterable[Int]]]
    implicitly[Monoid[Seq[Int]]]
  }

  def zero {
    implicitly[Zero[Map[Int, Int]]]
  }

  def monad[A, B, R, S, T, U, V, W, X] {
    implicitly[Monad[List]]
    implicitly[Monad[Stream]]
    implicitly[Monad[NonEmptyList]]
    implicitly[Monad[PartialApply1Of2[State, A]#Apply]]
    implicitly[Monad[Function0]]
    implicitly[Monad[PartialApply1Of2[Function1, R]#Apply]]
    implicitly[Monad[PartialApply2Of3[Function2, R, S]#Apply]]
    implicitly[Monad[PartialApply3Of4[Function3, R, S, T]#Apply]]
    implicitly[Monad[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    implicitly[Monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    implicitly[Monad[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]]
    implicitly[Monad[PartialApply1Of2[Either.LeftProjection, X]#Flip]]
    implicitly[Monad[PartialApply1Of2[Either.RightProjection, X]#Apply]]
    import java.util.Map.Entry    
    implicitly[Monad[PartialApply1Of2[Entry, Int]#Apply]]
    implicitly[Monad[PartialApply1Of2[Validation, X]#Apply]]
    implicitly[Monad[PartialApply1Of2[FailProjection, X]#Flip]]
  }

  def functor {
    implicitly[Functor[java.util.PriorityQueue]]
  }

  def pure {
    implicitly[Pure[Iterable]]
    implicitly[Pure[List]]
    implicitly[Pure[Vector]]
  }

  def foldRight[A] {
    implicitly[Foldable[Stream]]
    implicitly[Foldable[List]]
    implicitly[Foldable[Set]]
  }

  def partialApply {
    trait A
    trait B
    trait C
    trait D
    trait E
    trait F
    trait G

    trait T1[A]
    trait T2[A, B]
    trait T3[A, B, C]
    trait T4[A, B, C, D]
    trait T5[A, B, C, D, E]
    trait T6[A, B, C, D, E, F]
    trait T7[A, B, C, D, E, F, G]

    implicitly[PartialApply1Of2[T2, A]#Apply[B] =:= T2[A, B]]
    implicitly[PartialApply1Of2[T2, A]#Flip[B] =:= T2[B, A]]

    implicitly[PartialApply2Of3[T3, A, B]#Apply[C] =:= T3[A, B, C]]
    implicitly[PartialApply2Of3[T3, A, B]#ApplyA[C] =:= T3[C, A, B]]
    implicitly[PartialApply2Of3[T3, A, B]#ApplyB[C] =:= T3[A, C, B]]

    implicitly[PartialApply3Of4[T4, A, B, C]#Apply[D] =:= T4[A, B, C, D]]

    implicitly[PartialApply4Of5[T5, A, B, C, D]#Apply[E] =:= T5[A, B, C, D, E]]

    implicitly[PartialApply5Of6[T6, A, B, C, D, E]#Apply[F] =:= T6[A, B, C, D, E, F]]

    implicitly[PartialApply6Of7[T7, A, B, C, D, E, F]#Apply[G] =:= T7[A, B, C, D, E, F, G]]
  }
}
