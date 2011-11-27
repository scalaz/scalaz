package scalaz

object Scalaz
  extends IdInstances with StateFunctions
  with syntax.ToAllTypeClassV // syntax associated with type classes
  with syntax.ToAllOtherV     // syntax associated with Scalaz data structures
  with std.AllInstances       // Type class instances for the standard library types
  with std.AllFunctions       // Functions related to standard library types
  with syntax.std.ToAllStdV   // syntax associated with standard library types
