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
          val chars = (0 until arity).map(n => ('A' + n).toChar.toString).toList
          val tparams = chars.mkString(", ")
          val params = (1 to arity).map("_" + _).mkString(", ")
          val ztparams = (1 to arity).map(_ => "Z").mkString(", ")
          val mapallTParams = chars.map(x => double(x.toString)).mkString(", ")
          val mapallParams = chars.zipWithIndex map { case (c, i) => "_%d: (%s => %s) = identity[%s] _".format(i + 1, c, double(c), c) } mkString(", ")
          val mapallApply = chars.zipWithIndex map{ case (c, i) => "_%d(value._%d)".format(i + 1, i + 1) } mkString(", ")

          val copyParams = chars.zipWithIndex map { case (c, i) => "_%d: %s = value._%d".format(i + 1, double(c), i + 1) } mkString(", ")
          val copyApply = chars.zipWithIndex map{ case (c, i) => "_%d".format(i + 1) } mkString(", ")
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