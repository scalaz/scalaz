package fjs.test.reflect

import fj.test.{Rand, CheckResult}
import fj.test.reflect.CheckParams
import fjs.data.List._
import fjs.data.Option._
import fj.{P2, P3}
import fjs.P2._
import fjs.P3._

object Check {
  private implicit def checkResults(r: fj.data.List[P2[String, CheckResult]]): List[(String, CheckResult)] =
    (r: List[(P2[String, CheckResult])]) map (t => t)

  def check[A](c: List[Class[A]], r: Rand, categories: String*): List[(String, CheckResult)] =
    fj.test.reflect.Check.check(c, r, categories.toArray: _*)

  def check[A](c: List[Class[A]], categories: String*): List[(String, CheckResult)] =
    fj.test.reflect.Check.check(c, categories.toArray: _*)

  def check[A](c: Class[A]*): List[(String, CheckResult)] =
    fj.test.reflect.Check.check(c.toList, new Array[String](0): _*)

  def check[A](r: Rand, c: Class[A]*): List[(String, CheckResult)] =
    fj.test.reflect.Check.check(c.toList, r, new Array[String](0): _*)

  def properties[A](c: Class[A], categories: String*): List[(Property, String, Option[CheckParams])] =
      (fj.test.reflect.Check.properties(c, categories.toArray: _*): List[P3[fj.test.Property, String, fj.data.Option[CheckParams]]]) map (p => (p._1, p._2, p._3))
}
