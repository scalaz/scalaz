package object scalaz
    extends BaseTypeclasses
    with BaseData
    with BaseDataAliases
    with BaseTypes
    with BaseCt
    with BaseAlgebra
    with BaseCore {

  // Types
  type Boolean = scala.Boolean
  type Byte    = scala.Byte
  type Double  = scala.Double
  type Float   = scala.Float
  type Int     = scala.Int
  type Long    = scala.Long
  type Short   = scala.Short
  type String  = java.lang.String
  type Unit    = scala.Unit

  // don't export this, but scala needs `StringContext` to be in scope with this name
  private[scalaz] val StringContext = scala.StringContext

  // Functions
  @scala.inline
  def identity[T](t: T): T = t
  @scala.inline
  def implicitly[T](implicit t: T) = t
}
