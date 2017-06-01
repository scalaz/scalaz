
object Util {
  def initLower(s: String) = {
    val (init, rest) = s.splitAt(1)
    init.toLowerCase + rest
  }
}
