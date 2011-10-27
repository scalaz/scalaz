package scalaz
package std

trait TypeConstraints {
  val <:< = new Category[<:<] {
    def id[A]: <:<[A, A] = conforms[A]
    def compose[A, B, C](f: B <:< C, g: A <:< B): A <:< C = f.asInstanceOf[A <:< C]
  }

  val =:= = new Category[=:=] {
    def id[A]: =:=[A, A] = implicitly
    def compose[A, B, C](f: B =:= C, g: A =:= B): A =:= C = f.asInstanceOf[A =:= C]
  }
}

object typeConstraint extends TypeConstraints
