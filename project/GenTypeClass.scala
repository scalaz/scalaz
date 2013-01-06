import java.lang.String
import sbt._

case class TypeClass(name: String, kind: Kind, pack: Seq[String] = Seq("scalaz"), extendsList: Seq[TypeClass] = Seq()) {
  require(pack.head == "scalaz")
  def syntaxPack = {
    Seq("scalaz", "syntax") ++ pack.drop(1)
  }

  def packageString0 = pack.map("package " + _).mkString("\n")
  def packageString = pack.mkString(".")
  def fqn = (pack :+ name).mkString(".")
  def doc = "[[" + fqn + "]]" + (if (extendsList.nonEmpty) " extends " + extendsList.map(tc => "[[" + tc.fqn + "]]").mkString(" with ") else "")
}

object TypeClass {
  import Kind._

  lazy val semigroup = TypeClass("Semigroup", *)
  lazy val monoid = TypeClass("Monoid", *, extendsList = Seq(semigroup))
  lazy val group = TypeClass("Group", *, extendsList = Seq(monoid))
  lazy val equal = TypeClass("Equal", *)
  lazy val show = TypeClass("Show", *)
  lazy val order = TypeClass("Order", *, extendsList = Seq(equal))
  lazy val enum = TypeClass("Enum", *, extendsList = Seq(order))
  lazy val metricSpace = TypeClass("MetricSpace", *)

  lazy val length = TypeClass("Length", *->*)
  lazy val each = TypeClass("Each", *->*)
  lazy val index = TypeClass("Index", *->*)
  lazy val functor = TypeClass("Functor", *->*)
  lazy val apply: TypeClass = TypeClass("Apply", *->*, extendsList = Seq(functor))
  lazy val applicative = TypeClass("Applicative", *->*, extendsList = Seq(apply))
  lazy val zip = TypeClass("Zip", *->*)
  lazy val unzip = TypeClass("Unzip", *->*)
  lazy val bind = TypeClass("Bind", *->*, extendsList = Seq(apply))
  lazy val monad = TypeClass("Monad", *->*, extendsList = Seq(applicative, bind))
  lazy val foldable = TypeClass("Foldable", *->*)
  lazy val foldable1 = TypeClass("Foldable1", *->*, extendsList = Seq(foldable))
  lazy val traverse = TypeClass("Traverse", *->*, extendsList = Seq(functor, foldable))
  lazy val traverse1 = TypeClass("Traverse1", *->*, extendsList = Seq(traverse, foldable1))

  lazy val contravariant = TypeClass("Contravariant", *->*)
  lazy val cojoin = TypeClass("Cojoin", *->*, extendsList = Seq(functor))
  lazy val cobind = TypeClass("Cobind", *->*, extendsList = Seq(functor))
  lazy val comonad = TypeClass("Comonad", *->*, extendsList = Seq(cojoin, cobind))
  lazy val cozip = TypeClass("Cozip", *->*)
  lazy val codiagonal = TypeClass("Codiagonal", *^*->*)

  lazy val plus = TypeClass("Plus", *->*, extendsList = Seq())
  lazy val plusEmpty = TypeClass("PlusEmpty", *->*, extendsList = Seq(plus))
  lazy val isEmpty = TypeClass("IsEmpty", *->*, extendsList = Seq(plusEmpty))

  lazy val applicativePlus = TypeClass("ApplicativePlus", *->*, extendsList = Seq(applicative, plusEmpty))
  lazy val monadPlus = TypeClass("MonadPlus", *->*, extendsList = Seq(monad, applicativePlus))

  lazy val bifunctor = TypeClass("Bifunctor", *^*->*)
  lazy val bifoldable = TypeClass("Bifoldable", *^*->*)
  lazy val bitraverse = TypeClass("Bitraverse", *^*->*, extendsList = Seq(bifunctor, bifoldable))
  lazy val compose = TypeClass("Compose", *^*->*)
  lazy val category = TypeClass("Category", *^*->*, extendsList = Seq(compose))
  lazy val choice = TypeClass("Choice", *^*->*, extendsList = Seq(category))
  lazy val split = TypeClass("Split", *^*->*, extendsList = Seq(category))
  lazy val first = TypeClass("First", *^*->*)
  lazy val arrow = TypeClass("Arrow", *^*->*, extendsList = Seq(category))

  lazy val liftIO = TypeClass("LiftIO", *->*, pack = Seq("scalaz", "effect"))
  lazy val monadIO = TypeClass("MonadIO", *->*, extendsList = Seq(liftIO, monad), pack = Seq("scalaz", "effect"))
  lazy val liftControlIO = TypeClass("LiftControlIO", *->*, pack = Seq("scalaz", "effect"))
  lazy val monadControlIO = TypeClass("MonadControlIO", *->*, extendsList = Seq(liftControlIO, monad), pack = Seq("scalaz", "effect"))
  lazy val resource = TypeClass("Resource", *, pack = Seq("scalaz", "effect"))

  //   Not automatically generated.
  //  lazy val monadState = TypeClass("MonadState", *^*->*, monad)

  def core: List[TypeClass] = List(semigroup,
    monoid,
    group,
    equal,
    length,
    show,
    order,
    enum,
    metricSpace,
    plusEmpty,
    isEmpty,
    each,
    index,
    functor,
    contravariant,
    apply,
    applicative,
    zip,
    unzip,
    cozip,
    bind,
    monad,
    cojoin,
    cobind,
    comonad,
    plus,
    applicativePlus,
    monadPlus,
    foldable,
    foldable1,
    traverse,
    traverse1,
    bifunctor,
    bifoldable,
    bitraverse,
    compose,
    category,
    choice,
    split,
    arrow
  )
  lazy val concurrent = Seq[TypeClass]()
  lazy val xml = Seq[TypeClass]()
  def effect = Seq(liftIO, monadIO, liftControlIO, monadControlIO, resource)
}

sealed abstract class Kind

object Kind {

  case object * extends Kind

  case object *->* extends Kind
  
  case object *^*->* extends Kind
}

object GenTypeClass {
  val useDependentMethodTypes = true

  case class SourceFile(packages: Seq[String], fileName: String, source: String) {
    def file(scalaSource: File): File = packages.foldLeft(scalaSource)((file, p) => file / p) / fileName

    def createOrUpdate(scalaSource: File, log: Logger): sbt.File = {
      val f = file(scalaSource)
      val updatedSource = if (f.exists()) {
        val old = IO.read(f)
        log.info("Updating %s".format(f))
        updateSource(old)
      } else {
        log.info("Creating %s".format(f))
        source
      }
      log.debug("Contents: %s".format(updatedSource))
      IO.delete(f)
      IO.write(f, updatedSource)
      f
    }

    def updateSource(oldSource: String): String = {
      val delimiter = "////"
      def parse(text: String): Seq[String] = {
        text.split(delimiter)
      }
      val oldChunks: Seq[String] = parse(oldSource)
      val newChunks: Seq[String] = parse(source)
      if (oldChunks.length != newChunks.length) sys.error("different number of chunks in old and new source: " + fileName)

      val updatedChunks = for {
        ((o, n), i) <- oldChunks.zip(newChunks).zipWithIndex
      } yield {
        val useOld = i % 2 == 1
        if (useOld) o else n
      }
      updatedChunks.mkString(delimiter)
    }
  }

  case class TypeClassSource(mainFile: SourceFile, syntaxFile: SourceFile) {
    def sources = List(mainFile, syntaxFile)
  }

  def typeclassSource(tc: TypeClass): TypeClassSource = {
    val typeClassName = tc.name
    val kind = tc.kind
    val extendsList = tc.extendsList.toList.map(_.name)

    import TypeClass._
    val classifiedTypeIdent = if (Set(arrow, category, choice, split, compose, codiagonal)(tc)) "=>:"
    else "F"

    val typeShape: String = kind match {
      case Kind.*      => ""
      case Kind.*->*   => "[_]"
      case Kind.*^*->* => "[_, _]"
    }
    val classifiedType = classifiedTypeIdent +  typeShape

    val classifiedTypeF = "F" +  typeShape

    def extendsListText(suffix: String, parents: Seq[String] = extendsList, cti: String = classifiedTypeIdent) = parents match {
      case Seq() => ""
      case es    => es.map(n => n + suffix + "[" + cti + "]").mkString("extends ", " with ", "")
    }
    def extendsToSyntaxListText = kind match {
      case Kind.*->* | Kind.*^*->* =>
        "extends To" + typeClassName + "Ops0" + (extendsList match {
          case Seq() => ""
          case es    => es.map(n => "To" + n + "Ops").mkString(" with ", " with ", "")
        })
      case _    =>
        extendsList match {
          case Seq() => ""
          case es    => es.map(n => "To" + n + "Ops").mkString("extends ", " with ", "")
        }
    }
    val extendsLikeList = extendsListText("")

    val syntaxPackString = tc.syntaxPack.map("package " + _).mkString("\n") + (if (tc.pack == Seq("scalaz")) "" else "\n\n" + "import " + (tc.pack :+ tc.name).mkString("."))
    val syntaxPackString1 = tc.syntaxPack.mkString(".")
    val syntaxMember = "val %sSyntax = new %s.%sSyntax[%s] { def F = %s.this }".format(Util.initLower(typeClassName), syntaxPackString1, typeClassName, classifiedTypeIdent, typeClassName)

    val mainSource = """%s

////
/**
 *
 */
////
trait %s[%s] %s { self =>
  ////

  // derived functions

  ////
  %s
}

object %s {
  @inline def apply[%s](implicit F: %s[F]): %s[F] = F

  ////

  ////
}
""".format(tc.packageString0, typeClassName, classifiedType, extendsLikeList, syntaxMember,
      typeClassName,
      classifiedTypeF, typeClassName, typeClassName, typeClassName, classifiedTypeIdent, classifiedTypeIdent
      )
    val mainSourceFile = SourceFile(tc.pack, typeClassName + ".scala", mainSource)

    val syntaxSource = kind match {
      case Kind.* =>
        """%s

/** Wraps a value `self` and provides methods related to `%s` */
trait %sOps[F] extends Ops[F] {
  implicit def F: %s[F]
  ////

  ////
}

trait To%sOps %s {
  implicit def To%sOps[F](v: F)(implicit F0: %s[F]) =
    new %sOps[F] { def self = v; implicit def F: %s[F] = F0 }

  ////

  ////
}

trait %sSyntax[F] %s {
  implicit def To%sOps(v: F): %sOps[F] = new %sOps[F] { def self = v; implicit def F: %s[F] = %sSyntax.this.F }
  
  def F: %s[F]
  ////

  ////
}
""".format(syntaxPackString, typeClassName, typeClassName, typeClassName, typeClassName, extendsToSyntaxListText,

      // implicits in ToXxxSyntax
      typeClassName, typeClassName, typeClassName, typeClassName,
      
      // trait MonadSyntax[F] extends ... { ... } 
      typeClassName, extendsListText("Syntax", cti = "F"),
      typeClassName, typeClassName, typeClassName, typeClassName, typeClassName,
      
      typeClassName
    )
    case Kind.*->* =>
      val ToVUnapply =
"""  implicit def To%sOpsUnapply[FA](v: FA)(implicit F0: Unapply[%s, FA]) =
    new %sOps[F0.M,F0.A] { def self = F0(v); implicit def F: %s[F0.M] = F0.TC }
""".format(Seq.fill(4)(typeClassName): _*)
      val ToVMA =
"""  implicit def To%sOps[F[_],A](v: F[A])(implicit F0: %s[F]) =
    new %sOps[F,A] { def self = v; implicit def F: %s[F] = F0 }
""".format(Seq.fill(4)(typeClassName) :_*)

    """%s

/** Wraps a value `self` and provides methods related to `%s` */
trait %sOps[F[_],A] extends Ops[F[A]] {
  implicit def F: %s[F]
  ////

  ////
}

trait To%sOps0 {
%s
}

trait To%sOps %s {
%s
  ////

  ////
}

trait %sSyntax[F[_]] %s {
  implicit def To%sOps[A](v: F[A]): %sOps[F, A] = new %sOps[F,A] { def self = v; implicit def F: %s[F] = %sSyntax.this.F }

  def F: %s[F]
  ////

  ////
}
""".format(syntaxPackString, typeClassName, typeClassName,
      typeClassName,
      typeClassName, ToVUnapply,
      typeClassName, extendsToSyntaxListText,

          ToVMA,

          typeClassName, extendsListText("Syntax", cti = "F"),
          typeClassName, typeClassName, typeClassName, typeClassName, typeClassName, 
          
          typeClassName
        )
      case Kind.*^*->* =>

        val ToVUnapply =
  """  implicit def To%sOpsUnapply[FA](v: FA)(implicit F0: Unapply2[%s, FA]) =
      new %sOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: %s[F0.M] = F0.TC }
  """.format(Seq.fill(4)(typeClassName): _*)
       val ToVFAB =
  """
  implicit def To%sOps[F[_, _],A, B](v: F[A, B])(implicit F0: %s[F]) =
      new %sOps[F,A, B] { def self = v; implicit def F: %s[F] = F0 }
  """.format(Seq.fill(4)(typeClassName) :_*)


    """%s

/** Wraps a value `self` and provides methods related to `%s` */
trait %sOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: %s[F]
  ////

  ////
}

trait To%sOps0 {
  %s
}

trait To%sOps %s {
  %s

  ////

  ////
}

trait %sSyntax[F[_, _]] %s {
  implicit def To%sOps[A, B](v: F[A, B]): %sOps[F, A, B] = new %sOps[F, A, B] { def self = v; implicit def F: %s[F] = %sSyntax.this.F }

  def F: %s[F]
  ////

  ////
}
""".format(syntaxPackString, typeClassName, typeClassName, typeClassName,
          typeClassName,
          ToVUnapply,
          typeClassName, extendsToSyntaxListText,
          ToVFAB,
          typeClassName, extendsListText("Syntax", cti = "F"),
          typeClassName, typeClassName, typeClassName, typeClassName, typeClassName, 
          
          typeClassName
        )
    }
    val syntaxSourceFile = SourceFile(tc.syntaxPack, typeClassName + "Syntax.scala", syntaxSource)
    TypeClassSource(mainSourceFile, syntaxSourceFile)
  }
}
