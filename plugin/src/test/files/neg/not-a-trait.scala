import scalaz.meta.minimal

@minimal("foo")
class Šķira {
  def foo: Int = 1
}

@minimal("foo")
object Priekšmets {
  def foo: Int = 1
}
