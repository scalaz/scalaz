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

  case object Traverse extends TypeClass {
    def gen(typename: String): String =
      """|
      |  implicit def %sTraverse: Traverse[%s] = new Traverse[%s] {
      |    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: %s[A]): F[%s[B]] =
      |      as.foldr[F[%s[B]]]((%s.empty[B]) η)((x, ys) => implicitly[Apply[F]].apply(f(x) ∘ ((a: B) => (b: %s[B]) => a +: b), ys))
      |  }
      |""".stripMargin.format(identifierFor(typename), typename, typename, typename, typename, typename, typename, typename)
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

  val mainTypeClasses = Seq(Pure, Empty, Functor, Bind, Foldable, Zero, Semigroup, Plus)
  val allTypeClasses = Seq(Pure, Empty, Functor, Bind, Foldable, Zero, Semigroup, Plus, Traverse)

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
    "collection.IndexedSeq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.Iterable" -> mainTypeClasses,
    "collection.LinearSeq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.Seq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.Set" -> mainTypeClasses,
    "collection.SortedSet" -> Seq(),
    "collection.Traversable" -> mainTypeClasses,
    "collection.TraversableOnce" -> Seq(),
    "collection.immutable.IndexedSeq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.immutable.Iterable" -> (mainTypeClasses),
    "collection.immutable.LinearSeq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.immutable.List" -> Seq(Pure, Empty, Functor, Bind, Zero, Semigroup, Plus),
    "collection.immutable.PagedSeq" -> Seq(),
    "collection.immutable.Queue" -> mainTypeClasses,
    "collection.immutable.Seq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.immutable.Set" -> mainTypeClasses,
    "collection.immutable.Stream" -> Seq(Pure, Empty, Functor, Bind, Zero, Semigroup, Plus, Traverse),
    "collection.immutable.Vector" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.mutable.ArrayBuffer" -> (mainTypeClasses),
    "collection.mutable.ArraySeq" -> mainTypeClasses,
    "collection.mutable.ArrayStack" -> Seq(),
    "collection.mutable.Buffer" -> mainTypeClasses,
    "collection.mutable.DoubleLinkedList" -> mainTypeClasses,
    "collection.mutable.HashSet" -> mainTypeClasses,
    "collection.mutable.IndexedSeq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.mutable.Iterable" -> mainTypeClasses,
    "collection.mutable.LinearSeq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.mutable.LinkedHashSet" -> mainTypeClasses,
    "collection.mutable.LinkedList" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.mutable.ListBuffer" -> mainTypeClasses,
    "collection.mutable.MutableList" -> Seq(),
    "collection.mutable.PriorityQueue" -> Seq(),
    "collection.mutable.Queue" -> Seq(Pure),
    "collection.mutable.Seq" -> (mainTypeClasses ++ Seq(Traverse)),
    "collection.mutable.Set" -> mainTypeClasses,
    "collection.mutable.Stack" -> Seq(),
    "collection.mutable.Traversable" -> mainTypeClasses
    )
}
