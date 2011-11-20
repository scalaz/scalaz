package scalaz

import org.specs2.matcher._
import org.specs2.mutable.FragmentsBuilder
import org.specs2.specification.{Example, Fragments, BaseSpecification, SpecificationStructure}

/** A minimal version of the Specs2 mutable base class */
trait Spec
  extends BaseSpecification with FragmentsBuilder with MustExpectations
  with MustThrownExpectations with ShouldThrownExpectations with ScalaCheckMatchers
  with MatchersImplicits with StandardMatchResults  {

  def is = specFragments

  def be_===[T: Show : Equal](expected: T): Matcher[T] = new Matcher[T] {
    def apply[S <: T](actual: Expectable[S]): MatchResult[S] = {
      val actualT = actual.value.asInstanceOf[T]
      def test = Equal[T].equal(expected, actualT)
      def koMessage = "%s != %s".format(Show[T].shows(expected), Show[T].shows(actualT))
      def okMessage = "%s == %s".format(Show[T].shows(expected), Show[T].shows(actualT))
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }
}