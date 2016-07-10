package scalaz.example

object NameNeedValueUsage extends App {
  import scalaz._

  def undefined = sys.error("!!")

  class Inc {
    var i = 0

    def apply(): Int = {
      i += 1
      i
    }
  }

  Need(undefined)

  Name(undefined)

  {
    val inc = new Inc
    val n = Name(inc.apply())
    assert(n.value == 1)
    assert(n.value == 2)
  }

  {
    val inc = new Inc
    val n = Need(inc.apply())
    assert(n.value == 1)
    assert(n.value == 1)
  }

  {
    val inc = new Inc
    val n = Value(inc.apply())
    assert(n.value == 1)
    assert(n.value == 1)
  }

  {
    import Name.name._
    val inc = new Inc
    assert(bind(Name(inc.apply()))(x => Name(x)).value == 1)
  }

  {
    import Need.need._
    val inc = new Inc
    assert(bind(Need(inc.apply()))(x => Need(x)).value == 1)
  }

}
