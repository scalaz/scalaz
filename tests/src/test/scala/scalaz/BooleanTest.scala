package scalaz

import std.AllInstances._

class BooleanTest extends Spec {
  "boolean functions" in {

    import scalaz.std.{boolean => b}

    "and" ! prop { (p:Boolean, q:Boolean) =>
      b.conjunction(p, q) == (p && q)
    }

    "or" ! prop { (p:Boolean, q:Boolean) =>
      b.disjunction(p, q) == (p || q)
    }

    "nand" ! prop { (p:Boolean, q:Boolean) =>
      b.nand(p, q) == !(p && q)
    }

    "nor" ! prop { (p:Boolean, q:Boolean) =>
      b.nor(p, q) == !(p || q)
    }

    "conditional" ! prop { (p:Boolean, q:Boolean) =>
      b.conditional(p, q)  == (!p || q)
    }

    "inverse conditional" ! prop { (p:Boolean, q:Boolean) =>
      b.inverseConditional(p, q)  == (p || !q)
    }

    "negate conditional" ! prop { (p:Boolean, q:Boolean) =>
      b.negConditional(p, q)  == (p && !q)
    }

    "negate inverse conditional" ! prop { (p:Boolean, q:Boolean) =>
      b.negInverseConditional(p, q) == (!p && q)
    }
  }
}
