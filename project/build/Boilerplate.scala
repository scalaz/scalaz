import sbt._
import xsbt.FileUtilities.write

trait Boilerplate {
  self: DefaultProject =>

  def srcManagedScala = "src_managed" / "main" / "scala"

  lazy val generateTupleW = {
    val cleanSrcManaged = cleanTask(srcManagedScala) named ("clean src_managed")
    task {
      val arities = 2 to 22

      def writeFileScalazPackage(fileName: String, source: String): Unit = {
        val file = (srcManagedScala / "scalaz" / fileName).asFile
        write(file, source)
      }

      def double(s: String) = s + s

      val tuples = for (arity: Int <- arities) yield {
        val tupleWSource: String = {
          case class N(n: Int) {
            val alpha: String = ('A' + (n - 1)).toChar.toString
            val alpha2: String = alpha + alpha
            val element: String = "_" + n
          }
          val ns = (1 to arity) map N.apply
          def mapMkString(f: N => String): String = ns.map(f).mkString(", ")

          val tparams = mapMkString {n => n.alpha}
          val params = mapMkString {n => n.element}

          val ztparams = mapMkString {_ => "Z"}

          val mapallTParams = mapMkString { n => n.alpha2 }
          val mapallParams = mapMkString { n => "%s: (%s => %s) = identity[%s] _".format(n.element, n.alpha, n.alpha2, n.alpha) }
          val mapallApply = mapMkString { n => "%s(value.%s)".format(n.element, n.element) }

          val pimp = """|
          |trait Tuple%dW[%s] extends PimpedType[Tuple%d[%s]] {
          |  def fold[Z](f: => (%s) => Z): Z = {import value._; f(%s)}
          |  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple%d[%s]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(%s)}
          |  def mapElements[%s](%s): (%s) = (%s)
          |}""".stripMargin.format(arity, tparams, arity, tparams, tparams, params, arity,
            ztparams, params,
            mapallTParams, mapallParams, mapallTParams, mapallApply
            )

          val conv = """|
          |implicit def ToTuple%dW[%s](t: (%s)): Tuple%dW[%s] = new { val value = t } with Tuple%dW[%s]
          |""".stripMargin.format(arity, tparams, tparams, arity, tparams, arity, tparams)
          pimp + "\n" + conv
        }
        tupleWSource
      }

      val source = "package scalaz\n\n" +
              "trait Tuples {\n" +
              "  " + tuples.map("  " +).mkString("\n") +
              "}"
      writeFileScalazPackage("TupleW.scala", source)
      None
    } dependsOn (cleanSrcManaged)
  }
}