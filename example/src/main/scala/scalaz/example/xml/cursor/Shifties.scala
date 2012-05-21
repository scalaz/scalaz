package scalaz.example
package xml
package cursor

object Shifties {
  def main(args: Array[String]) {

    import scalaz._, Scalaz._
    import scalaz.xml._, cursor._, Xml._

    // XML Data. Note unbalanced tag and unquoted attribute.
    val xml = """<yo><!-- let's go! --><blib>hello<chook egg=no>tennisball</chook><chook blah='asdf'>toaster</blib></yo>"""

    // Parse the XML data into document results.
    val px = xml.parseXml

    // A cursor operation that renames the element at the current cursor position.
    def renameElem(n: String): Shift =
      shift(c =>
        hcursorc(cursor = c usingElem (_ setName n.toList)))

    // *** Perform zipper/cursor operations ***
    // 1. Advance cursor to first child element.
    // 2. Move cursor to the element to the right.
    // 3. Rename the element at the current cursor operation.
    // 4. Move cursor to the left if possible; to the right otherwise.
    // 5. Move cursor to the left if possible; to the right otherwise.
    // 6. Reset the cursor to the start of the document.
    // 7. Drop the cursor and produce the new document.
    val r = px map (
      firstChild >=> right >=> renameElem("renamed") >=> (left ||| right) >=> (left ||| right) >=> root run _)

    // For each parse result
    r foreach {
      h => {
        // Pretty-print the new document.
        // (tags will be balanced, attributes quoted, cursor updates performed).
        -h foreach (_ xprintln pretty)
        // Print the history of cursor operations.
        h.history.println
      }
    }

  }
}
