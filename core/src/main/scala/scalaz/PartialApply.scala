package scalaz

/**
 * Scala doesn't currently have a way to directly express a
 * type function, such as `[A]Either[A, Int]`. Instead, we must
 * pass type constructor and fixed types as arguments to one
 * of the traits below. For example, the type function mentioned
 * above is expressed as `PartialApply1Of2[Either, Int]#Flip`
 *
 * This can also be expressed as `({type λ[α]=Either[α, Int]})#λ`.
*/
trait PartialApply1Of2[T[_, _], A] {
  type Apply[B] = T[A, B]

  type Flip[B] = T[B, A]
}

trait PartialApply1Of3[T[_, _, _], A] {
  type Apply[B,C] = T[A, B, C]
}

trait PartialApply2Of3[T[_, _, _], A, B] {
  type Apply[C] = T[A, B, C]

  type ApplyB[C] = T[A, C, B]

  type ApplyA[C] = T[C, A, B]
}

trait PartialApply3Of4[T[_, _, _, _], A, B, C] {
  type Apply[D] = T[A, B, C, D]
}

trait PartialApply4Of5[T[_, _, _, _, _], A, B, C, D] {
  type Apply[E] = T[A, B, C, D, E]
}

trait PartialApply5Of6[T[_, _, _, _, _, _], A, B, C, D, E] {
  type Apply[F] = T[A, B, C, D, E, F]
}

trait PartialApply6Of7[T[_, _, _, _, _, _, _], A, B, C, D, E, F] {
  type Apply[G] = T[A, B, C, D, E, F, G]
}

trait PartialApplyK[T[_[_], _, _], M[_]] {
  type Apply[A, B] = T[M, A, B]
}

trait PartialApplyKA[T[_[_], _, _], M[_], A] {
  type Apply[B] = T[M, A, B]

  type Flip[B] = T[M, B, A]
}

/**
 * Applies one type argument of two where one of the type arguments has a kind * -> *.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
trait PartialType2[T[_[_], _], A[_]] {
  type Apply[B] = T[A, B]
}
