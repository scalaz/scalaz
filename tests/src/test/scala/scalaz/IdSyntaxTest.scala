package scalaz

import std.AllInstances._

object IdSyntaxTest extends SpecLite {
  "smorgasbord" in {
    import syntax.id._
    2 |> (_ * 3) must_===(6)

    1.squared must_===((1, 1))

    "foo" matchOrZero {
      case "bar" => 1
    } must_===(0)

    0.doWhile(_ + 1, _ < 3) must_===(3)

    0.doWhile(_ + 1, _ => false) must_===(1)

    0.whileDo(_ + 1, _ < 3) must_===(3)

    0.whileDo(_ + 1, _ => false) must_===(0)
  }

}
