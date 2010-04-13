package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import Scalaz._

class ListWTest extends Specification with Sugar with ScalaCheck {
  "intersperse" verifies {
    def intersperse[A](value: List[A], a: A): List[A] = value match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case h :: t => h :: a :: intersperse(t, a)
    }
    (a: List[Int], b: Int) => (a.intersperse(b) ≟ intersperse(a, b))
  }

  "intercalate" verifies {
    def intercalate[A](value: List[A], as: List[A]): List[A] = value match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case h :: t => h :: as ::: intercalate(t, as)
    }
    (a: List[Int], b: List[Int]) => (a.intercalate(b) ≟ intercalate(a, b))
  }
}
