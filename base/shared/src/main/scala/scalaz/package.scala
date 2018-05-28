package object scalaz
    extends BaseTypeclasses
    with BaseData
    with BaseDataAliases
    with BaseTypes
    with BaseCt
    with BaseAlgebra
    with BaseCore {

  // Types
  type Boolean          = scala.Boolean
  type Byte             = scala.Byte
  type Double           = scala.Double
  type Float            = scala.Float
  type Function[-A, +B] = (A) ⇒ B
  type Int              = scala.Int
  type Long             = scala.Long
  type Short            = scala.Short
  type Singleton        = scala.Singleton
  type String           = java.lang.String
  type Unit             = scala.Unit

  type <:<[-A, +B] = scala.Predef.<:<[A, B]
  type =:=[A, B]   = scala.Predef.=:=[A, B]

  type inline = scala.inline

  // Objects
  val StringContext = scala.StringContext

  // Functions
  def identity[T](t: T)            = scala.Predef.identity[T](t)
  def implicitly[T](implicit t: T) = scala.Predef.implicitly[T](t)
}
