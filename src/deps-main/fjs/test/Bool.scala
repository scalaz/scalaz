package fjs.test

sealed abstract class Bool {
  val b: fj.test.Bool

  def ->(p: => fj.test.Property) = b implies new fj.P1[fj.test.Property] {
    def _1 = p
  }
}

object Bool {
  implicit def Boolean_Bool(b: Boolean) = fj.test.Bool.bool(b)
  implicit def Bool_Boolean(b: fj.test.Bool) = b.is

  implicit def Boolean_SBool(bb: Boolean): Bool = new Bool {
    val b = (bb: fj.test.Bool)
  }
  implicit def SBool_Boolean(b: Bool) = b.b.is

  implicit def SBool_Bool(b: Bool) = b.b
  implicit def Bool_SBool(bb: fj.test.Bool): Bool = new Bool {
    val b = bb
  }
}
