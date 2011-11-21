package scalaz
package std

trait AllFunctions
  extends ListFunctions
  with OptionFunctions
  with StreamFunctions
  with math.OrderingFunctions
  with StringFunctions

object AllFunctions extends AllFunctions