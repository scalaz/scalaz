package scalaz

import std.AllInstances._

class BooleanSyntaxTest extends Spec {
  "boolean syntax" in {
    import syntax.id._
    import syntax.std.boolean._

    "and" ! prop { (p:Boolean, q:Boolean) =>
      p /\ q == (p && q)
    }

    "or" ! prop { (p:Boolean, q:Boolean) =>
      p \/ q == (p || q)
    }

    "nand" ! prop { (p:Boolean, q:Boolean) =>
      p !&& q == !(p && q)
    }

    "nor" ! prop { (p:Boolean, q:Boolean) =>
      p !|| q == !(p || q)
    }

    "conditional" ! prop { (p:Boolean, q:Boolean) =>
      p --> q == (!p || q)
    }

    "inverse conditional" ! prop { (p:Boolean, q:Boolean) =>
      p <-- q == (p || !q)
    }

    "negate conditional" ! prop { (p:Boolean, q:Boolean) =>
      p -/> q == (p && !q)
    }

    "negate inverse conditional" ! prop { (p:Boolean, q:Boolean) =>
      p <\- q == (!p && q)
    }

    "true.option" ! prop { (i: Int) =>
      true.option(i).exists(_ == i)
    }

    "false.option" ! prop { (i: Int) =>
      false.option(i).isEmpty
    }

    "boolean.whenM" ! prop { (b: Boolean) =>
      b.whenM(None).isDefined != b
    }

    "boolean.unlessM" ! prop { (b: Boolean) =>
      b.unlessM(None).isDefined == b
    }

    "boolean.guard" ! prop { (b: Boolean, s: String) =>
      b.guard[Option](s) == b.option(s)
    }

    "boolean.prevent" ! prop { (b: Boolean, s: String) =>
      b.prevent[Option](s) == (!b).option(s)
    }

    "true.??" ! prop { (s: String) =>
      true ?? s == s
    }

    "false.??" ! prop { (s: String) =>
      false ?? s == implicitly[Monoid[String]].zero
    }

    "true.!?" ! prop { (s: String) =>
      true !? s == implicitly[Monoid[String]].zero
    }

    "false.!?" ! prop { (s: String) =>
      false !? s == s
    }
  }
}
