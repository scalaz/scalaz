package scalaz
package std

trait AllFunctions
  extends ListFunctions
  with OptionFunctions
  with LazyListFunctions
  with BooleanFunctions
  with math.OrderingFunctions
  with StringFunctions
  with FunctionFunctions

object AllFunctions extends AllFunctions
