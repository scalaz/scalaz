package scalaz

object Example {
  def run {
    ExampleApplicative.run
    ExampleBifunctor.run
    ExampleCategory.run
    ExampleCofunctor.run
    ExampleDistance.run
    ExampleFunctor.run
    ExampleKleisli.run
    ExampleIdentity.run
    ExampleMonad.run
    ExampleMonoid.run
    ExamplePlus.run
    ExampleTraverse.run
    geo.ExampleVincenty.run
  }

  def main(args: Array[String]) = run
}
