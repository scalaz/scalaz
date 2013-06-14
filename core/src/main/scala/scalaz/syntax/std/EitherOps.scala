package scalaz
package syntax
package std

trait EitherOps[A, B] extends Ops[Either[A, B]] {

  final def disjunction: A \/ B = \/ fromEither self
}

trait ToEitherOps {
  implicit def ToEitherOpsFromEither[A, B](e: Either[A, B]): EitherOps[A, B] = new EitherOps[A, B] {
    val self = e
  }
}

