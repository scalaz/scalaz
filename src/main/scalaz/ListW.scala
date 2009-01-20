package scalaz

sealed trait ListW[A] {
  val list: List[A]  
}

object ListW {
  implicit def ListWList[A](as: ListW[A]) = as.list

  implicit def ListListW[A](as: List[A]) = new ListW[A] {
    val list = as
  }
}
