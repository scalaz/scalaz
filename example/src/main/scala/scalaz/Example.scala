package scalaz

object Example {
  def run {
    ExampleApplicative.run
    ExampleArrow.run
    ExampleBifunctor.run
    ExampleBKTree.run
    ExampleCategory.run
    ExampleCofunctor.run
    ExampleDistance.run
    ExampleFunctor.run
    ExampleFold.run
    ExampleEqual.run
    ExampleKleisli.run
    ExampleIdentity.run
    ExampleMonad.run
    ExampleMonoid.run
    ExamplePlus.run
    ExampleState.run
    ExampleTraverse.run
    geo.ExampleVincenty.run
    math.ExampleFAD.run
    concurrent.ExampleActor.run
  }

  def main(args: Array[String]) = run
}
