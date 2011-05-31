package scalaz.example


object Example {
  def run {
    ExampleApplicative.run
    ExampleArrow.run
    ExampleBifunctor.run
    ExampleCategory.run
    ExampleContravariant.run
    ExampleComp.run
    ExampleDistance.run
    ExampleFunctor.run
    ExampleFold.run
    ExampleEqual.run
    ExampleMonad.run
    ExampleMonoid.run
    ExamplePlus.run
    ExampleTraverse.run
    geo.ExampleVincenty.run
    concurrent.ExampleActor.run
    concurrent.HammerTime.run
    WordCount.wordCount
  }

  def main(args: Array[String]) = run
}
