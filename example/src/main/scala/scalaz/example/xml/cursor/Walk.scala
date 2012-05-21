package scalaz.example
package xml
package cursor

object Walk {
  def main(args: Array[String]) {

    import scalaz.xml._, cursor._, Xml._

    // XML document
    val xml =
"""<html lang="en">
  <head>
    <title>Walk example</title>
  </head>
  <body>
    <!-- xyz -->
    <p id="p1">First paragraph</p>
    <p id="p2">Second paragraph</p>
    <deleteattr attr="this attribute will be deleted" keep="this one is kept">
      The attr attribute on this element will be deleted.
    </deleteattr>
    <div id="d1" attr="this attribute is kept.">
      <p id="p3">Third paragraph</p>
      <delete description="this element will be deleted"/>
    </div>
  </body>
</html>"""

    // Parse the XML data into document results.
    val px = xml.parseXml

    // Run this function at each position in the XML document,
    // replacing the current position with the return result.
    def atCursor(c: Cursor): Content = {
      val d =
        // Delete any elements named "delete".
        if(c.elem exists (_.sname == "delete"))
          c iremoveOr c
        // Delete any attributes named "attr" in elements named "deleteattr".
        else if(c.elem exists (_.sname == "deleteattr"))
          c usingElem (_ removeAttrsKeySname "attr")
        else
          c
      d.current
    }

    // Walk the XML document applying the function
    // to operate at the current cursor position.
    val r = px map (_ walk atCursor)

    // Pretty-print the result.
    r foreach (_ xprintln pretty)

  }
}
