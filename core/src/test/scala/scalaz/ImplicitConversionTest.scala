package scalaz

// compiles == green
object ImplicitConversionTest {
  import scalaz.Scalaz._

  def MAs[A, B] {
    implicitly[List[A] <%%< MA[List, A]]
    implicitly[Option[A] <%%< MA[Option, A]]

    implicitly[(A => B) <%%< MACofunctor[PartialApply1Of2[Function1, B]#Flip, A]]
    implicitly[(A => B) <%%< MA[PartialApply1Of2[Function1, A]#Apply, B]]

    implicitly[(Validation[A, B]) <%%< MA[PartialApply1Of2[Validation, A]#Apply, B]]

    // todo
    //implicitly[(FailProjection[A, B]) <%%< MA[PartialApply1Of2[Validation, B]#Flip, A]]

    // via higher kind inference
    trait T[A]
    implicitly[T[A] <%%< MACofunctor[T, A]]
    implicitly[T[A] <%%< MA[T, A]]
  }

  def apply {
    implicitly[Apply[Identity]]
    implicitly[Apply[List]]
    implicitly[Apply[Function0]]
    implicitly[Apply[Option]]
    implicitly[Apply[PartialApply1Of2[Function1, Int]#Apply]]
  }

  def bind {
    implicitly[Bind[List]]
    implicitly[Bind[GArray]]
    implicitly[Bind[java.util.ArrayList]]
  }

  def monad {
    implicitly[Monad[List]]
    implicitly[Monad[Stream]]
  }

  def functor {
    implicitly[Functor[java.util.PriorityQueue]]
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

    implicitly[PartialApply1Of2[T2, A]#Apply[B] =::= T2[A, B]]
    implicitly[PartialApply1Of2[T2, A]#Flip[B] =::= T2[B, A]]

    implicitly[PartialApply2Of3[T3, A, B]#Apply[C] =::= T3[A, B, C]]
    implicitly[PartialApply2Of3[T3, A, B]#ApplyA[C] =::= T3[C, A, B]]
    implicitly[PartialApply2Of3[T3, A, B]#ApplyB[C] =::= T3[A, C, B]]

    implicitly[PartialApply3Of4[T4, A, B, C]#Apply[D] =::= T4[A, B, C, D]]

    implicitly[PartialApply4Of5[T5, A, B, C, D]#Apply[E] =::= T5[A, B, C, D, E]]

    implicitly[PartialApply5Of6[T6, A, B, C, D, E]#Apply[F] =::= T6[A, B, C, D, E, F]]

    implicitly[PartialApply6Of7[T7, A, B, C, D, E, F]#Apply[G] =::= T7[A, B, C, D, E, F, G]]
  }
}