import sbt._
import xsbt.FileUtilities.write

trait Boilerplate {
  self: DefaultProject =>

  def srcManagedScala = "src_managed" / "main" / "scala"

  sealed trait TypeClass {
    def gen(typename: String): String
  }

  case object Pure extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sPure[A]: Pure[%s] = new Pure[%s] {
      |    def pure[A](a: => A): %s[A] = %s(a)
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename, typename)
  }

  case object Empty extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sEmpty[A]: Empty[%s] = new Empty[%s] {
      |    def empty[A]: %s[A] = %s()
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename, typename)
  }

  case object Functor extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sFunctor[A]: Functor[%s] = new Functor[%s] {
      |    def fmap[A, B](r: %s[A], f: A => B) = r map f
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename)
  }

  case object Bind extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sBind[A]: Bind[%s] = new Bind[%s] {
      |    def bind[A, B](r: %s[A], f: A => %s[B]) = r flatMap f
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename, typename)
  }

  case object Foldable extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sFoldable[A]: Foldable[%s] = new Foldable[%s] {
      |    override def foldRight[A, B](t: %s[A], b: => B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
      |    override def foldLeft[A, B](t: %s[A], b: B, f: (B, A) => B): B = t.foldLeft(b)(f(_, _))
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename, typename)
  }

  case object Zero extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sZero[A]: Zero[%s[A]] = zero(%s())
      |""".stripMargin.format(identifierFor(typename), typename, typename)
  }

  case object Semigroup extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sSemigroup[A]: Semigroup[%s[A]] = semigroup(_ ++ _)
      |""".stripMargin.format(identifierFor(typename), typename)
  }

  case object Plus extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sPlus[A] = new Plus[%s] {
      |    def plus[A](x: %s[A], y: => %s[A]): %s[A] = x ++ y
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename, typename)
  }

  val allTypeClasses = Seq(Pure, Empty, Functor, Bind, Foldable, Zero, Semigroup, Plus)

  lazy val generateCollectionTypeClassInstances = {
    val cleanSrcManaged = cleanTask(srcManagedScala) named ("clean src_managed")
    task {
      for{
        tc <- allTypeClasses
      } {
        val source = generateObjectSource(tc)
        val fileName = tc.toString + "Collections.scala"
        val file = (srcManagedScala / "scalaz" / fileName).asFile
        write(file, source)
        ()
      }
      None
    } dependsOn (cleanSrcManaged)
  }

  def generateObjectSource(typeclass: TypeClass) = {
    val header =
    """|package scalaz
    |
    |// generated during SBT build in Boilerplate.scala
    |trait %sCollections {
    |  import Scalaz._
    |
    |""".stripMargin.format(typeclass.toString)
    val footer =
    """
    |}""".stripMargin

    val body = collections.filter(_._2.contains(typeclass)).map(_._1).map(typeclass.gen(_)).mkString("\n")

    header + body + footer
  }

  /**
   * collection.Abc => Abc
   * collection.immutable.Abc => ImmutableAbc
   */
  private def identifierFor(typeName: String) = {
    val R1 = """collection\.(\w+)""".r
    val R2 = """collection\.(\w+)\.(\w+)""".r
    typeName match {
      case R1(x) => x
      case R2(p1, x) => p1.capitalize + x
    }
  }

  val collections = Seq(
    "collection.BufferedIterator" -> Seq(),
    "collection.IndexedSeq" -> allTypeClasses,
    "collection.Iterable" -> allTypeClasses,
    "collection.LinearSeq" -> allTypeClasses,
    "collection.Seq" -> allTypeClasses,
    "collection.Set" -> allTypeClasses,
    "collection.SortedSet" -> Seq(),
    "collection.Traversable" -> allTypeClasses,
    "collection.TraversableOnce" -> Seq(),
    "collection.immutable.IndexedSeq" -> allTypeClasses,
    "collection.immutable.Iterable" -> allTypeClasses,
    "collection.immutable.LinearSeq" -> allTypeClasses,
    "collection.immutable.List" ->  Seq(Pure, Empty, Functor, Bind, Zero, Semigroup, Plus),
    "collection.immutable.PagedSeq" -> Seq(),
    "collection.immutable.Queue" -> allTypeClasses,
    "collection.immutable.Seq" -> allTypeClasses,
    "collection.immutable.Set" -> allTypeClasses,
    "collection.immutable.Stream" -> allTypeClasses,
    "collection.immutable.Vector" -> allTypeClasses,
    "collection.mutable.ArrayBuffer" -> allTypeClasses,
    "collection.mutable.ArraySeq" -> allTypeClasses,
    "collection.mutable.ArrayStack" -> Seq(),
    "collection.mutable.Buffer" -> allTypeClasses,
    "collection.mutable.DoubleLinkedList" -> allTypeClasses,
    "collection.mutable.HashSet" -> allTypeClasses,
    "collection.mutable.IndexedSeq" -> allTypeClasses,
    "collection.mutable.Iterable" -> allTypeClasses,
    "collection.mutable.LinearSeq" -> allTypeClasses,
    "collection.mutable.LinkedHashSet" -> allTypeClasses,
    "collection.mutable.LinkedList" -> allTypeClasses,
    "collection.mutable.ListBuffer" -> allTypeClasses,
    "collection.mutable.MutableList" -> Seq(),
    "collection.mutable.PriorityQueue" -> Seq(),
    "collection.mutable.Queue" -> Seq(Pure),
    "collection.mutable.Seq" -> allTypeClasses,
    "collection.mutable.Set" -> allTypeClasses,
    "collection.mutable.Stack" -> Seq(),
    "collection.mutable.Traversable" -> allTypeClasses
  )
}
