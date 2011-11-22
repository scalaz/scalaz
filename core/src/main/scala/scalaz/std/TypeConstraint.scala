package scalaz
package std

trait TypeConstraintInstances {
  val <:< = new Category[<:<] {
    def id[A] = conforms[A]
    def compose[A, B, C](f: B <:< C, g: A <:< B) = f.asInstanceOf[A <:< C]
  }

  val =:= = new Category[=:=] {
    def id[A] = implicitly
    def compose[A, B, C](f: B =:= C, g: A =:= B) = f.asInstanceOf[A =:= C]
  }
}

object typeConstraint extends TypeConstraintInstances
