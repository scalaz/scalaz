package scalaz.example
package xml

object SimpleTokens {
  val in = """<html lang="en"><head><div><div>wibble</div></div></head><body>simple</body><w:wibble><div></div></w:wibble></html>"""
  import scalaz.xml.Xml._

  def main(args: Array[String]) {
    val r = in.parseXml
    r foreach (_.xprint(pretty.shortEmptyTagOff))
  }
}