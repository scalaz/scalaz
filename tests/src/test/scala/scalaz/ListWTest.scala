package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import Scalaz._

class ListWTest extends Specification with Sugar with ScalaCheck {
  "intercalate empty list is identity" verifies {a: List[Int] => a.intercalate(nil) ≟ a}

  "intersperse then remove odd items is identity" verifies {(a: List[Int], b: Int) =>
    val isEven = (_: Int) % 2 == 0
    a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) ≟ a
  }

  "intercalate single element lists the same as intersperse" verifies {(a: List[Int], b: Int) =>
    a.intercalate(List(b)) ≟ a.intersperse(b)
  }

  "intersperse vs benchmark" verifies {
    def intersperse[A](value: List[A], a: A): List[A] = value match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case h :: t => h :: a :: intersperse(t, a)
    }
    (a: List[Int], b: Int) => (a.intersperse(b) ≟ intersperse(a, b))
  }

  "intercalate vs benchmark" verifies {
    def intercalate[A](value: List[A], as: List[A]): List[A] = value match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case h :: t => h :: as ::: intercalate(t, as)
    }
    (a: List[Int], b: List[Int]) => (a.intercalate(b) ≟ intercalate(a, b))
  }

  "groupNelBy produces NonEmptyLists of the correct number of elements" verifies {
    (ss: List[String]) =>  {
      val grouped = ss.groupNelBy(_.length)
      grouped.values.foldMap(_.count) === ss.length
    }
  }

  "groupNelBy produces NonEmptyLists of the correct elements" verifies {
    (ss: List[String]) =>  {
      val grouped = ss.groupNelBy(_.length)
      grouped.values.map(_.list).flatten.toSet === ss.toSet
    }
  }

  "groupByM joined (in Identity) produces the same list" verifies {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupByM[Identity]((a, b) => p(a, b).η[Identity]).value.join === a
  }
}
