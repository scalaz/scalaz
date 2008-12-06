package fjs.data

final case class List[A](as: fj.data.List[A]) {
  def sort(implicit o: fj.pre.Ord[A]) = as.sort(o)
}

import fj.data.List.{nil, cons}
import fj.Function.curry
import F2._

object List {
  implicit def ScalaList_List[A](as: scala.List[A]): fj.data.List[A] =
    as.foldRight(nil[A])(cons(_, _))

  implicit def List_ScalaList[A](as: fj.data.List[A]): scala.List[A] =
    as.foldRight(curry((_ :: _): (A, scala.List[A]) => scala.List[A]), Nil)

  implicit def ScalaList_SList[A](as: scala.List[A]): List[A] = List(as)

  implicit def SList_ScalaList[A](as: List[A]): scala.List[A] = as.as

  implicit def List_SList[A](as: fj.data.List[A]): List[A] = List(as)

  implicit def SList_List[A](as: List[A]): fj.data.List[A] = as.as

  object :| {
    def unapply[A](as: fj.data.List[A]) =
      if(as.isEmpty) None
      else Some(as.head, as.tail)
  }
}
