package scalaz

object Scalaz
  extends Idents with States with Readers
  with syntax.ToAllTypeClassV // syntax associated with type classes
  with std.AllInstances       // Type class instances for the standard library types
  with std.AllFunctions       // Functions related to standard library types
  with syntax.std.ToAllStdV   // syntax associated with standard library types
