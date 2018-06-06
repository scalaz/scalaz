import scalaz.meta.minimal

@minimal("foo", "bar", "baz")
trait nonexistent {
  var bar = 1
  val baz = 2
}