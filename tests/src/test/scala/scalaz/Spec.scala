package scalaz

import org.specs2.matcher._
import org.specs2.mutable.FragmentsBuilder
import org.specs2.specification.{Example, Fragments, BaseSpecification, SpecificationStructure}
import org.specs2.main.{ArgumentsShortcuts, ArgumentsArgs}
import org.scalacheck.{Prop, Properties}

/** A minimal version of the Specs2 mutable base class */
trait Spec
  extends BaseSpecification with FragmentsBuilder with MustExpectations
  with MustThrownExpectations with ShouldThrownExpectations with ScalaCheckMatchers
  with MatchersImplicits with StandardMatchResults
  with ArgumentsShortcuts with ArgumentsArgs {

  addArguments(fullStackTrace)

  def is = specFragments

  addArguments(fullStackTrace)

  def be_===[T: Show : Equal](expected: T): Matcher[T] = new Matcher[T] {
    def apply[S <: T](actual: Expectable[S]): MatchResult[S] = {
      val actualT = actual.value.asInstanceOf[T]
      def test = Equal[T].equal(expected, actualT)
      def koMessage = "%s != %s".format(Show[T].shows(expected), Show[T].shows(actualT))
      def okMessage = "%s == %s".format(Show[T].shows(expected), Show[T].shows(actualT))
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }

  override implicit val defaultParameters = Parameters(defaultValues.updated(maxSize, 5).updated(minTestsOk, 33))

  def checkAll(name: String, props: Properties)(implicit p: Parameters) {
    addFragments(name,
      for ((name, prop) <- props.properties) yield { name in check(prop)(p)}
      , "must satisfy"
    )
  }

  implicit def enrichProperties(props: Properties) = new {
    def withProp(propName: String, prop: Prop) = new Properties(props.name) {
      for {(name, p) <- props.properties} property(name) = p
      property(propName) = prop
    }
  }
}