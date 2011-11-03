package scalaz.std

trait AllInstances
  extends AnyValInstances with FunctionInstances with ListInstances
  with OptionInstances with StringInstances with StreamInstances with TupleInstances
  with EitherInstances with PartialFunctionInstances with TypeConstraintInstances
  with scalaz.std.math.BigDecimalInstances with scalaz.std.math.BigInts
  with scalaz.std.util.parsing.combinator.Parsers
  with scalaz.std.java.util.MapInstances

object AllInstances extends AllInstances