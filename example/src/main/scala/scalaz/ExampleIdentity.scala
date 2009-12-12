package scalaz

object ExampleIdentity {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    1: Identity[Int]
    1.wrapNel assert_≟ NonEmptyList(1)
  }
}
