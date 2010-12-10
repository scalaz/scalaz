package scalaz

// compiles == green
object ImplicitConversionTest {
  import Scalaz._
  import Predef.{implicitly => i}

  // Ambiguous implicits with 2.8.1
  trait F[S,A] {
    type M[B] = State[S,B]
    i[Applicative[M]]
  }

  def MAs[A, B, C, D, E, F, G, H] {
    i[List[A] <%< MA[List, A]]
    i[Option[A] <%< MA[Option, A]]
    i[(A => B) <%< MACofunctor[({type λ[α]=(α) => B})#λ, A]]
    i[(A => B) <%< MA[({type λ[α]=(A) => α})#λ, B]]
    i[((A, B) => C) <%< MA[PartialApply2Of3[Function2, A, B]#Apply, C]]
    i[((A, B, C) => D) <%< MA[PartialApply3Of4[Function3, A, B, C]#Apply, D]]
    i[((A, B, C, D) => E) <%< MA[PartialApply4Of5[Function4, A, B, C, D]#Apply, E]]
    i[((A, B, C, D, E) => F) <%< MA[PartialApply5Of6[Function5, A, B, C, D, E]#Apply, F]]
    i[((A, B, C, D, E, F) => G) <%< MA[({type λ[α]=(A, B, C, D, E, F) => α})#λ, G]]
    i[Validation[A, B] <%< MA[({type λ[α]=Validation[A, α]})#λ, B]]
    i[FailProjection[A, B] <%< MA[({type λ[α]=FailProjection[α, B]})#λ, A]]
    i[Either.LeftProjection[A, B] <%< MA[({type λ[α]=Either.LeftProjection[α, B]})#λ, A]]
    i[Either.RightProjection[A, B] <%< MA[({type λ[α]=Either.RightProjection[A, α]})#λ, B]]
    i[(A, B) <%< MA[({type λ[α]=(A, α)})#λ, B]]
    i[(A, B, C) <%< MA[PartialApply2Of3[Tuple3, A, B]#Apply, C]]
    i[(A, B, C, D) <%< MA[PartialApply3Of4[Tuple4, A, B, C]#Apply, D]]
    i[(A, B, C, D, E) <%< MA[PartialApply4Of5[Tuple5, A, B, C, D]#Apply, E]]
    i[(A, B, C, D, E, F) <%< MA[PartialApply5Of6[Tuple6, A, B, C, D, E]#Apply, F]]
    i[(A, B, C, D, E, F, G) <%< MA[({type λ[α]=(A, B, C, D, E, F, α)})#λ, G]]

    // via higher kind inference
    trait T[A]
    i[T[A] <%< MACofunctor[T, A]]
    i[T[A] <%< MA[T, A]]
  }

  def apply[A, B, R, S, T, U, V, W, X] {
    i[Apply[Identity]]
    i[Apply[List]]
    i[Apply[Function0]]
    i[Apply[Option]]
    i[Apply[({type λ[α]=State[A, α]})#λ]]
    i[Apply[Function0]]
    i[Apply[({type λ[α]=(R) => α})#λ]]
    i[Apply[PartialApply2Of3[Function2, R, S]#Apply]]
    i[Apply[PartialApply3Of4[Function3, R, S, T]#Apply]]
    i[Apply[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    i[Apply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    i[Apply[({type λ[α]=(R, S, T, U, V, W) => α})#λ]]
    i[Apply[({type λ[α]=Either.LeftProjection[α, X]})#λ]]
    i[Apply[({type λ[α]=Either.RightProjection[X, α]})#λ]]
    import java.util.Map.Entry
    i[Apply[({type λ[α]=Entry[Int, α]})#λ]]
    i[Apply[({type λ[α]=Validation[Int, α]})#λ]]
    i[Apply[({type λ[α]=FailProjection[α, X]})#λ]]
  }

  def applicative[A, B, R, S, T, U, V, W, X] {
    i[Applicative[Identity]]
    i[Applicative[List]]
    i[Applicative[Function0]]
    i[Applicative[Option]]
    //i[Applicative[({type λ[α]=State[A, α]})#λ]]
    i[Applicative[Function0]]
    i[Applicative[({type λ[α]=(R) => α})#λ]]
    i[Applicative[PartialApply2Of3[Function2, R, S]#Apply]]
    i[Applicative[PartialApply3Of4[Function3, R, S, T]#Apply]]
    i[Applicative[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    i[Applicative[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    i[Applicative[({type λ[α]=(R, S, T, U, V, W) => α})#λ]]
    i[Applicative[({type λ[α]=Either.LeftProjection[α, X]})#λ]]
    i[Applicative[({type λ[α]=Either.RightProjection[X, α]})#λ]]
    import java.util.Map.Entry
    i[Applicative[({type λ[α]=Entry[Int, α]})#λ]]
    i[Applicative[({type λ[α]=Validation[Int, α]})#λ]]
    i[Applicative[({type λ[α]=FailProjection[α, X]})#λ]]
    i[Applicative[({type λ[α] = State[A, α]})#λ]]
  }

  def pointed[A, B, R, S, T, U, V, W, X] {
    i[Pointed[Identity]]
    i[Pointed[List]]
    i[Pointed[Function0]]
    i[Pointed[Option]]
    i[Pointed[({type λ[α]=State[A, α]})#λ]]
    i[Pointed[Function0]]
    i[Pointed[({type λ[α]=(R) => α})#λ]]
    i[Pointed[PartialApply2Of3[Function2, R, S]#Apply]]
    i[Pointed[PartialApply3Of4[Function3, R, S, T]#Apply]]
    i[Pointed[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    i[Pointed[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    i[Pointed[({type λ[α]=(R, S, T, U, V, W) => α})#λ]]
    i[Pointed[({type λ[α]=Either.LeftProjection[α, X]})#λ]]
    i[Pointed[({type λ[α]=Either.RightProjection[X, α]})#λ]]
    import java.util.Map.Entry
    i[Pointed[({type λ[α]=Entry[Int, α]})#λ]]
    i[Pointed[({type λ[α]=Validation[Int, α]})#λ]]
    i[Pointed[({type λ[α]=FailProjection[α, X]})#λ]]
  }

  def bind {
    i[Bind[List]]
    i[Bind[ArraySeq]]
    i[Bind[java.util.ArrayList]]
  }

  def monoid {
    i[Monoid[List[Int]]]
    i[Monoid[Iterable[Int]]]
    i[Monoid[Seq[Int]]]
  }

  def zero {
    i[Zero[Map[Int, Int]]]
  }

  def monad[A, B, R, S, T, U, V, W, X] {
    i[Monad[List]]
    i[Monad[Stream]]
    i[Monad[NonEmptyList]]
    i[Monad[({type λ[α]=State[A, α]})#λ]]
    i[Monad[Function0]]
    i[Monad[({type λ[α]=(R) => α})#λ]]
    i[Monad[PartialApply2Of3[Function2, R, S]#Apply]]
    i[Monad[PartialApply3Of4[Function3, R, S, T]#Apply]]
    i[Monad[PartialApply4Of5[Function4, R, S, T, U]#Apply]]
    i[Monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]]
    i[Monad[({type λ[α]=(R, S, T, U, V, W) => α})#λ]]
    i[Monad[({type λ[α]=Either.LeftProjection[α, X]})#λ]]
    i[Monad[({type λ[α]=Either.RightProjection[X, α]})#λ]]
    import java.util.Map.Entry    
    i[Monad[({type λ[α]=Entry[Int, α]})#λ]]
    i[Monad[({type λ[α]=Validation[X, α]})#λ]]
    i[Monad[({type λ[α]=FailProjection[α, X]})#λ]]
  }

  def functor {
    i[Functor[java.util.PriorityQueue]]
  }

  def pure {
    i[Pure[Iterable]]
    i[Pure[List]]
    i[Pure[Vector]]
  }

  def foldRight[A] {
    i[Foldable[Stream]]
    i[Foldable[List]]
    i[Foldable[Set]]
  }

  def equal {
    type A = Int
    type B = Int
    i[Equal[(A, B)]]
    i[Equal[Seq[A]]]
    i[Equal[List[A]]]
    i[Equal[ArraySeq[A]]]
    i[Equal[Stream[A]]]
    i[Equal[Map[A, B]]]
    i[Equal[collection.SortedMap[A, B]]]
    i[Equal[java.util.Map[A, B]]]
    i[Equal[java.util.List[A]]]
    i[Equal[java.lang.Iterable[A]]]
    i[Equal[IntMultiplication]]
  }

  def show {
    type A = Int
    type B = Int
    i[Show[(A, B)]]
    i[Show[Seq[A]]]
    i[Show[List[A]]]
    i[Show[ArraySeq[A]]]
    i[Show[Stream[A]]]
    i[Show[Map[A, B]]]
    i[Show[collection.SortedMap[A, B]]]
    i[Show[java.util.Map[A, B]]]
    i[Show[java.util.List[A]]]
    i[Show[java.lang.Iterable[A]]]
    i[Show[IntMultiplication]]
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

    i[({type λ[α]=T2[A, α]})#λ[B] =:= T2[A, B]]
    i[({type λ[α]=T2[α, A]})#λ[B] =:= T2[B, A]]

    i[PartialApply2Of3[T3, A, B]#Apply[C] =:= T3[A, B, C]]
    i[PartialApply2Of3[T3, A, B]#ApplyA[C] =:= T3[C, A, B]]
    i[PartialApply2Of3[T3, A, B]#ApplyB[C] =:= T3[A, C, B]]

    i[PartialApply3Of4[T4, A, B, C]#Apply[D] =:= T4[A, B, C, D]]

    i[PartialApply4Of5[T5, A, B, C, D]#Apply[E] =:= T5[A, B, C, D, E]]

    i[PartialApply5Of6[T6, A, B, C, D, E]#Apply[F] =:= T6[A, B, C, D, E, F]]

    i[({type λ[α]=T7[A, B, C, D, E, F, α]})#λ[G] =:= T7[A, B, C, D, E, F, G]]
  }

  def copointed {
    type A = Int
    i[Copointed[({type λ[α]=(A, α)})#λ]]

    import java.util.Map.Entry
    i[Copointed[({type λ[α]=Entry[A, α]})#λ]]
  }

  def comonad {
    type A = Int
    i[Comonad[({type λ[α]=(A, α)})#λ]]
    import java.util.Map.Entry
    i[Comonad[({type λ[α]=Entry[A, α]})#λ]]
  }
  
  def strategy {
    i[concurrent.Strategy]
  }
}
