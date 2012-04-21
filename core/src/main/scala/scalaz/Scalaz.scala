package scalaz

object Scalaz
  extends StateFunctions
  with syntax.ToAllTypeClassOps // syntax associated with type classes
  with syntax.ToAllOtherOps     // syntax associated with Scalaz data structures
  with std.AllInstances       // Type class instances for the standard library types
  with std.AllFunctions       // Functions related to standard library types
  with syntax.std.ToAllStdOps   // syntax associated with standard library types
