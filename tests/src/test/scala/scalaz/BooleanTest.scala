package scalaz
import org.scalacheck.Prop.forAll


object BooleanTest extends SpecLite {
  "boolean functions" in {

    import scalaz.std.{boolean => b}

    "and" ! forAll { (p:Boolean, q:Boolean) =>
      b.conjunction(p, q) == (p && q)
    }

    "or" ! forAll { (p:Boolean, q:Boolean) =>
      b.disjunction(p, q) == (p || q)
    }

    "nand" ! forAll { (p:Boolean, q:Boolean) =>
      b.nand(p, q) == !(p && q)
    }

    "nor" ! forAll { (p:Boolean, q:Boolean) =>
      b.nor(p, q) == !(p || q)
    }

    "conditional" ! forAll { (p:Boolean, q:Boolean) =>
      b.conditional(p, q)  == (!p || q)
    }

    "inverse conditional" ! forAll { (p:Boolean, q:Boolean) =>
      b.inverseConditional(p, q)  == (p || !q)
    }

    "negate conditional" ! forAll { (p:Boolean, q:Boolean) =>
      b.negConditional(p, q)  == (p && !q)
    }

    "negate inverse conditional" ! forAll { (p:Boolean, q:Boolean) =>
      b.negInverseConditional(p, q) == (!p && q)
    }
  }
}
