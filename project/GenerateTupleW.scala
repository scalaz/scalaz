import sbt._

object GenerateTupleW {

  def apply(outputDir: File) = {
    val arities = 2 to 12

    def writeFileScalazPackage(fileName: String, source: String): File = {
      val file = (outputDir / "scalaz" / "syntax" / "std" / fileName).asFile
      IO.write(file, source)
      file
    }

    def double(s: String) = s + s

    val tuples: IndexedSeq[(String, String)] = for (arity: Int <- arities) yield {
      case class N(n: Int) {
        val alpha: String = ('A' + (n - 1)).toChar.toString
        val alpha2: String = alpha + alpha
        val element: String = "_" + n
      }
      val ns = (1 to arity) map N.apply
      def mapMkString(f: N => String): String = ns.map(f).mkString(", ")

      val tparams = mapMkString {
        n => n.alpha
      }
      val params = mapMkString {
        n => n.element
      }

      val ztparams = mapMkString {
        _ => "Z"
      }

      val mapallTParams = mapMkString {
        n => n.alpha2
      }
      val mapallParams = mapMkString {
        n => s"${n.element}: (${n.alpha} => ${n.alpha2}) = identity[${n.alpha}] _"
      }
      val mapallApply = mapMkString {
        n => s"${n.element}(value.${n.element})"
      }

      val pimp = s"""|
          |final class Tuple${arity}Ops[${tparams}](value: (${tparams})) {
          |  def fold[Z](f: => (${tparams}) => Z): Z = {import value._; f(${params})}
          |  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple${arity}[${ztparams}]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(${params})}
          |  def mapElements[${mapallTParams}](${mapallParams}): (${mapallTParams}) = (${mapallApply})
          |}""".stripMargin

      val conv = s"""implicit def ToTuple${arity}Ops[${tparams}](t: (${tparams})): Tuple${arity}Ops[${tparams}] = new Tuple${arity}Ops(t)\n"""
      (pimp, conv)
    }

    val source = "package scalaz\npackage syntax\npackage std\n\nimport collection.immutable.IndexedSeq\n\n" +
      tuples.map(_._1).mkString("\n") +
      "\n\ntrait ToTupleOps {\n" +
         tuples.map("  " + _._2).mkString("\n") +
      "}"
    writeFileScalazPackage("TupleOps.scala", source)
  }

}
