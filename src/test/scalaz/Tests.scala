package scalaz

import fjs.test.Property._
import fj.test.CheckResult.summaryEx

object Tests {
  val props = List(
        CheckEitherW.props,
        CheckOptionW.props,
        list.CheckNonEmptyList.props,
        control.CheckFunctor.props,
        control.CheckMonad.props,
        control.CheckMonadEmpty.props,
        control.CheckApplicative.props,
        control.CheckSemigroup.props,
        control.CheckMonoid.props
      ) flatMap(x => x)

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run

  import Equal._
}
