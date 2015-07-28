package scalaz

import std.AllInstances._
import org.scalacheck.Prop.forAll

object BooleanSyntaxTest extends SpecLite {
  "boolean syntax" in {
    import syntax.std.boolean._

    "and" ! forAll { (p:Boolean, q:Boolean) =>
      p /\ q == (p && q)
    }

    "or" ! forAll { (p:Boolean, q:Boolean) =>
      p \/ q == (p || q)
    }

    "nand" ! forAll { (p:Boolean, q:Boolean) =>
      p !&& q == !(p && q)
    }

    "nor" ! forAll { (p:Boolean, q:Boolean) =>
      p !|| q == !(p || q)
    }

    "conditional" ! forAll { (p:Boolean, q:Boolean) =>
      p --> q == (!p || q)
    }

    "inverse conditional" ! forAll { (p:Boolean, q:Boolean) =>
      p <-- q == (p || !q)
    }

    "bi-conditional" in {
      assert(false <--> false)
      assert(true <--> true)
      assert(!(true <--> false))
      assert(!(false <--> true))
    }

    "negate conditional" ! forAll { (p:Boolean, q:Boolean) =>
      p -/> q == (p && !q)
    }

    "negate inverse conditional" ! forAll { (p:Boolean, q:Boolean) =>
      p <\- q == (!p && q)
    }

    "true.option" ! forAll { (i: Int) =>
      true.option(i).exists(_ == i)
    }

    "false.option" ! forAll { (i: Int) =>
      false.option(i).isEmpty
    }

    "boolean.whenM" ! forAll { (b: Boolean) =>
      b.whenM(None).isDefined != b
    }

    "boolean.unlessM" ! forAll { (b: Boolean) =>
      b.unlessM(None).isDefined == b
    }

    "boolean.whenMU" ! forAll { (b: Boolean) =>
      import syntax.validation._
      b.whenMU("false".failure).isSuccess != b
    }

    "boolean.unlessMU" ! forAll { (b: Boolean) =>
      import syntax.validation._
      b.unlessMU("false".failure).isSuccess == b
    }

    "boolean.guard" ! forAll { (b: Boolean, s: String) =>
      b.guard[Option](s) == b.option(s)
    }

    "boolean.prevent" ! forAll { (b: Boolean, s: String) =>
      b.prevent[Option](s) == (!b).option(s)
    }

    "true.??" ! forAll { (s: String) =>
      true ?? s == s
    }

    "false.??" ! forAll { (s: String) =>
      false ?? s == implicitly[Monoid[String]].zero
    }

    "true.!?" ! forAll { (s: String) =>
      true !? s == implicitly[Monoid[String]].zero
    }

    "false.!?" ! forAll { (s: String) =>
      false !? s == s
    }
  }
}
