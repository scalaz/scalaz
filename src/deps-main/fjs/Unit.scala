package fjs

object Unit {
  implicit def ScalaUnit_Unit(u: scala.Unit) = fj.Unit.unit

  implicit def Unit_ScalaUnit(u: fj.Unit) = ()
}
