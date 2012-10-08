package scalaz

import std.AllInstances._

class BooleanSyntaxTest extends Spec {
  "boolean syntax" in {
    import syntax.id._
    import syntax.std.boolean._

    "true.option" ! check { (i: Int) =>
      true.option(i).exists(_ == i)
    }

    "false.option" ! check { (i: Int) =>
      false.option(i).isEmpty
    }

    "boolean.whenM" ! check { (b: Boolean) =>
      b.whenM(None).isDefined != b
    }

    "boolean.unlessM" ! check { (b: Boolean) =>
      b.unlessM(None).isDefined == b
    }

    "boolean.guard" ! check { (b: Boolean, s: String) =>
      b.guard[Option](s) == b.option(s)
    }

    "boolean.prevent" ! check { (b: Boolean, s: String) =>
      b.prevent[Option](s) == (!b).option(s)
    }

    "true.??" ! check { (s: String) =>
      true ?? s == s
    }

    "false.??" ! check { (s: String) =>
      false ?? s == implicitly[Monoid[String]].zero
    }

    "true.!?" ! check { (s: String) =>
      true !? s == implicitly[Monoid[String]].zero
    }

    "false.!?" ! check { (s: String) =>
      false !? s == s
    }
  }
}
