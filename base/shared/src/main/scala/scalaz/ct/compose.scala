package scalaz
package ct

import scala.language.experimental.macros

trait ComposeClass[=>:[_, _]] {
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)
}

trait ComposeFunctions {
  @inline final def compose[F[_, _], A, B, C](f: F[B, C], g: F[A, B])(implicit F: Compose[F]): F[A, C] =
    F.compose(f, g)
}

trait ComposeSyntax {
  implicit final class ToComposeOps[=>:[_, _], B, C](self: B =>: C) {
    def compose[A](f: A =>: B)(implicit ev: Compose[=>:]): A =>: C = macro meta.Ops.ia_1
  }
}
