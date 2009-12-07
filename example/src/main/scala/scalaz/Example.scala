package scalaz

object Example {
  def run {
    ExampleApplicative.run
    ExampleBifunctor.run
    ExampleCategory.run
    ExampleCofunctor.run
    ExampleFunctor.run
    ExampleKleisli.run
    ExampleMonad.run
    ExamplePlus.run
    ExampleTraverse.run
  }

  def main(args: Array[String]) = run
}
