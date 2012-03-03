package scalaz.example
package xml
package pp

object Tidy {
  val in =
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

  import scalaz.xml.Xml._

  def main(args: Array[String]) {
    val r = in.parseXml
    r foreach (_ xprint config())
  }
}