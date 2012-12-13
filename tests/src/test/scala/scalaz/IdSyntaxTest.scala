package scalaz

import std.AllInstances._

class IdSyntaxTest extends testlib.Spec {
  "smorgasbord" in {
    import syntax.id._
    2 |> (_ * 3) must be_===(6)

    1.squared must be_===((1, 1))

    "foo" matchOrZero {
      case "bar" => 1
    } must be_===(0)

    0.doWhile(_ + 1, _ < 3) must be_===(3)

    0.doWhile(_ + 1, _ => false) must be_===(1)

    0.whileDo(_ + 1, _ < 3) must be_===(3)

    0.whileDo(_ + 1, _ => false) must be_===(0)
  }

}