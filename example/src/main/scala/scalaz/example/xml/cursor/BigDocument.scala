package scalaz.example
package xml
package cursor


object BigDocument {
  def main(args: Array[String]) {

    import scalaz.xml._, cursor._, Xml._

    // XML document
    val xml =
"""<html lang="en">
  <head>
    <title>Big document example</title>
  </head>
  <body>""" +
    (1 to 200000 map (n => """<p>paragraph</p>
  <div id="div""" + n + """">
    <span>span</span>
  </div>""") mkString "\n") + """
  </body>
</html>"""

    // Parse the XML data into document results.
    val px = xml.parseXml

    // Run this function at each position in the XML document,
    // replacing the current position with the return result.
    def atCursor(c: Cursor): Content = {
      val d =
        // Make element names upper-case.
        c usingElem (_ withName (_ map (_.toUpper)))
        
      d.current
    }

    // Walk the XML document applying the function
    // to operate at the current cursor position.
    val r = px map (_ walk atCursor)

    // Pretty-print the result.
    r foreach (_ xprintln pretty)

  }
}