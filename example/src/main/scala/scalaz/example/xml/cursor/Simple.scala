package scalaz.example
package xml
package cursor

object Simple {
  val data =
"""<html lang="en">
  <head>
    <meta name="wibbles" content=wobbles>
  </head>
  <!-- this is a comment noting that body is an unbalanced element -->
  <body>
    <!-- unquoted attribute value -->
    <div class=blimby>
    </div>
    <!-- single-quoted attribute value -->
    <div class='blomby'>
    </div>
    <x:div class="blomby">
    <!-- unbalanced element -->
      <a href="http://google.com">search
    </x:div>
</html>"""

  import scalaz._, Scalaz._, xml.Xml._

  def main(args: Array[String]) {
    // Parse the document into a tree
    val r = data.parseXml

    val w =
      r map (c =>
        for {
          // Create a cursor and advance it to the first child that is a comment
          comment <- +c findChild (_.current.isComment)
          // Convert the contents of the comment to upper-case
          val u = comment -->> (_ usingComment (_ map (_.toUpper)))
          // Move the cursor back to the parent
          p <- u.^
          // Move the cursor to the first child element named <body>
          e <- p findChildElementName ("body" == _)
          // Move the cursor to the first child element named <x:div>
          h <- e findChild (_.current.elem exists (qnames(name = "div", prefix = Some("x".toList)) === _.name))
          // Move the cursor to the first child element named <a>
          anchor <- h findChildElementName ("a" == _)
          // Update the element with new attributes
          val a = anchor -->> (_ usingElem (_ *** ("id" -=- "modified_href", "href" -=- "http://yahoo.com/")))
        } yield -a
      )

    // Print the result after performing the update on an immutable data structure.
    w flatMap (_.toList) foreach (_.println)
  }
}