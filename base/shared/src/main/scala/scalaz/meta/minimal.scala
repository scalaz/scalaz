package scalaz
package meta

import com.github.ghik.silencer.silent

import scala.{ annotation, Any }

@silent
class minimal(defns: Any*) extends annotation.StaticAnnotation
