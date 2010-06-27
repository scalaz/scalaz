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

      for (arity: Int <- arities) {
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

          val copyParams = mapMkString { n => "%s: %s = value.%s".format(n.element, n.alpha2, n.element) }
          val copyApply = mapMkString { n => "%s".format(n.element) }
          val pimp = """|
          |trait Tuple%dW[%s] extends PimpedType[Tuple%d[%s]] {
          |  def fold[Z](f: => (%s) => Z): Z = {import value._; f(%s)}
          |  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple%d[%s]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(%s)}
          |  def mapAll[%s](%s): (%s) = (%s)
          |  def copy[%s](%s): (%s) = (%s)
          |}""".stripMargin.format(arity, tparams, arity, tparams, tparams, params, arity,
            ztparams, params,
            mapallTParams, mapallParams, mapallTParams, mapallApply,
            mapallTParams, copyParams, mapallTParams, copyApply
            )

          val conv = """|
          |trait Tuple%dWs {
          |  implicit def ToTuple%dW[%s](t: (%s)): Tuple%dW[%s] = new { val value = t } with Tuple%dW[%s]
          |}
          |""".stripMargin.format(arity, arity, tparams, tparams, arity, tparams, arity, tparams)
          pimp + "\n" + conv
        }

        val source = "package scalaz\n\n" + tupleWSource
        writeFileScalazPackage("Tuple%dWs.scala".format(arity), source)
      }

      val source = "package scalaz\n\n" + "trait TupleWs extends " + arities.map(n => "Tuple%dWs".format(n)).mkString("\n     with ")
      writeFileScalazPackage("TupleW.scala", source)
      None
    } dependsOn (cleanSrcManaged)
  }
}