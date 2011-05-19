package scalaz
package wrap

sealed trait IntW {

  import newtypes._

  val value: Int

  def multiplication: IntMultiplication =
    Pack.pack[Int, IntMultiplication](value)

  def ∏ : IntMultiplication =
    multiplication
}

object IntW extends IntWs


trait IntWs {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }
}