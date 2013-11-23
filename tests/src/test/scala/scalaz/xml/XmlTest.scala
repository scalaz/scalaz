package scalaz
package xml

import std.string._
import std.list._
import syntax.foldable._
import scalaz.xml.Xml._
import scalaz.xml.pp.Config
import scala.util.Random
import org.scalacheck.Prop.forAll

object XmlTest extends SpecLite {

  // https://github.com/scalaz/scalaz/issues/456
  "parse large CDATA" in {
    val data = {
      <cdata>{new scala.xml.PCData( Random.alphanumeric.take(1000000).mkString )}</cdata>
    }.toString

    data.parseXml.foldMap(_.sxprints(Config.config())) must_===(data)
  }

}
