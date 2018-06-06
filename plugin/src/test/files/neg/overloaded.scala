import scalaz.meta.minimal

@minimal("multiplicious")
trait overloaded {

  def multiplicious(i: Int)    = i
  def multiplicious(s: String) = s

}
