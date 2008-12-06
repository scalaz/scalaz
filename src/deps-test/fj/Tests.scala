package fj

import List.flatten

object Tests {
  def tests = flatten(List (
    fj.data.CheckArray.tests,
    fj.data.CheckList.tests,
    fj.data.CheckStream.tests,
    fj.data.CheckOption.tests,
    fj.data.CheckTree.tests,
    fj.data.CheckHashMap.tests,
    fj.data.CheckHashSet.tests,
    fj.data.CheckSet.tests,
    fj.control.parallel.CheckStrategy.tests
  ))

  def main(args: Array[String]) = run(tests)

  import org.scalacheck.Prop
  import org.scalacheck.ConsoleReporter._
  import org.scalacheck.Test
  import org.scalacheck.Test.{check, defaultParams}

  def run(tests: List[(String, Prop)]) =
    tests foreach { case (name, p) => {
        val c = check(defaultParams, p, (s, d) => {})
        c.status match {
          case Test.Passed => println("Passed " + name)
          case Test.Proved(_) => println("Proved " + name)
          case f @ Test.Failed(_, _) => error(name + ": " + f)
          case Test.Exhausted => println("Exhausted " + name)
          case f @ Test.GenException(e) => {
            e.printStackTrace
            error(name + ": " + f)
          }
          case f @ Test.PropException(_, e, _) => {
            e.printStackTrace
            error(name + ": " + f)
          }
        }
      }
    }
}
