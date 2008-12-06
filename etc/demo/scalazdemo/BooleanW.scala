package scalazdemo

import scalaz.BooleanW._

/*
false ~&& false
true

false ~&& true
true

true ~&& false
true

true ~&& true
false

false ~|| false
true

false ~|| true
false

true ~|| false
false

true ~|| true
false

false ==> false
true

false ==> true
true

true ==> false
false

true ==> true
true

false <== false
true

false <== true
false

true <== false
true

true <== true
true

false ~==> false
false

false ~==> true
false

true ~==> false
true

true ~==> true
false

false ~<== false
false

false ~<== true
true

true ~<== false
false

true ~<== true
false

true ? ("foo", "bar")
foo

false ? ("foo", "bar")
bar

true toOption ("foo")
Some(foo)

false toOption ("foo")
None

true toEither ("foo", "bar")
Right(bar)

false toEither ("foo", "bar")
Left(foo)

true !>
false unless
*/
object BooleanW {
  val demoes = List(
    // Negation of Conjunction
    ("false ~&& false", false ~&& false),
    ("false ~&& true", false ~&& true),
    ("true ~&& false", true ~&& false),
    ("true ~&& true", true ~&& true),

    // Negation of Disjunction
    ("false ~|| false", false ~|| false),
    ("false ~|| true", false ~|| true),
    ("true ~|| false", true ~|| false),
    ("true ~|| true", true ~|| true),

    // Conditional/Implication
    ("false ==> false", false ==> false),
    ("false ==> true", false ==> true),
    ("true ==> false", true ==> false),
    ("true ==> true", true ==> true),

    // Inverse Conditional/Implication
    ("false <== false", false <== false),
    ("false <== true", false <== true),
    ("true <== false", true <== false),
    ("true <== true", true <== true),

    // Negation of Conditional/Implication
    ("false ~==> false", false ~==> false),
    ("false ~==> true", false ~==> true),
    ("true ~==> false", true ~==> false),
    ("true ~==> true", true ~==> true),

    // Inverse Conditional/Implication
    ("false ~<== false", false ~<== false),
    ("false ~<== true", false ~<== true),
    ("true ~<== false", true ~<== false),
    ("true ~<== true", true ~<== true),

    // ?
    ("true ? (\"foo\", \"bar\")", true ? ("foo", "bar")),
    ("false ? (\"foo\", \"bar\")", false ? ("foo", "bar")),

    // toOption
    ("true toOption (\"foo\")", true toOption ("foo")),
    ("false toOption (\"foo\")", false toOption ("foo")),

    // toEither
    ("true toEither (\"foo\", \"bar\")", true toEither ("foo", "bar")),
    ("false toEither (\"foo\", \"bar\")", false toEither ("foo", "bar"))
  )

  def effectDemoes {
    // !>
    false !> println("false !>")
    true !> println("true !>")

    // unless
    false unless println("false unless")
    true unless println("true unless")
  }

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
    effectDemoes
  }
}
