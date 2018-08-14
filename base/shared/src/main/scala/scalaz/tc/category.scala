package scalaz
package tc

trait CategoryClass[=>:[_, _]] extends SemicategoryClass[=>:] {
  def id[A]: A =>: A
}

object CategoryClass {
  implicit def function1Category: Category[? => ?] = instanceOf(new CategoryClass[? => ?] {
    def id[A]: A => A = scala.Predef.identity[A]
    def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
  })
}
