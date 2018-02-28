package scalaz
package data

final case class Forget[A, B, C](forget: B => A) {
  def retag[D]: Forget[A, B, D] = this.asInstanceOf[Forget[A, B, D]]
}
