import sbt._

case class TypeClass(name: String, kind: Kind, extendsList: TypeClass*)

object TypeClass {
  import Kind._

  lazy val semigroup = TypeClass("Semigroup", *)
  lazy val monoid = TypeClass("Monoid", *, semigroup)
  lazy val equal = TypeClass("Equal", *)
  lazy val show = TypeClass("Show", *)
  lazy val order = TypeClass("Order", *, equal)

  lazy val functor = TypeClass("Functor", *->*)
  lazy val pointed = TypeClass("Pointed", *->*, functor)
  lazy val apply: TypeClass = TypeClass("Apply", *->*, functor)
  lazy val applicative = TypeClass("Applicative", *->*, apply, pointed)
  lazy val bind = TypeClass("Bind", *->*, apply)
  lazy val monad = TypeClass("Monad", *->*, applicative, bind)
  lazy val traverse = TypeClass("Traverse", *->*, functor)

  lazy val contravariant = TypeClass("Contravariant", *->*)
  lazy val copointed = TypeClass("Copointed", *->*, contravariant)
  lazy val cojoin = TypeClass("Cojoin", *->*)
  lazy val comonad = TypeClass("Comonad", *->*, copointed, cojoin)

  lazy val plus = TypeClass("Plus", *->*, functor)
  lazy val applicativePlus = TypeClass("ApplicativePlus", *->*, applicative, plus)
  lazy val monadPlus = TypeClass("MonadPlus", *->*, monad, applicativePlus)

  def all: List[TypeClass] = List(semigroup,
    monoid,
    equal,
    show,
    order,
    functor,
    pointed,
    contravariant,
    copointed,
    apply,
    applicative,
    bind,
    monad,
    cojoin,
    comonad,
    plus,
    applicativePlus,
    monadPlus,
    traverse)
}

sealed abstract class Kind

object Kind {

  case object * extends Kind

  case object *->* extends Kind

  def parse(s: String): Option[Kind] = s match {
    case "*" => Some(*)
    case "*->*" => Some(*->*)
    case _ => None
  }
}

object KindExtractor {
  def unapply(s: String): Option[Kind] = Kind.parse(s)
}

object GenTypeClass {

  case class SourceFile(packages: List[String], fileName: String, source: String) {
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
      if (oldChunks.length != newChunks.length) error("different number of chunks in old and new source: " + fileName)

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

  def typeclassSource(typeClassName: String, kind: Kind,
                      extendsList: Seq[String]): TypeClassSource = {
    val classifiedType = kind match {
      case Kind.* => "F"
      case Kind.*->* => "F[_]"
    }
    def initLower(s: String) = {
      val (init, rest) = s.splitAt(1)
      init.toLowerCase + rest
    }
    def extendsListText(suffix: String, parents: Seq[String] = extendsList) = parents match {
      case Seq() => ""
      case es => es.map(n => n + suffix + "[F]").mkString("extends ", " with ", "")
    }
    def extendsToSyntaxListText = extendsList match {
      case Seq() => ""
      case es => es.map(n => "To" + n + "Syntax").mkString("extends ", " with ", "")
    }
    val extendsLikeList = extendsListText("")
    val instanceList = extendsList.map(_ + "Instance")
    val (selfType, parentsDef) = ("", "")
    val extendsInstanceList = extendsListText("", typeClassName +: instanceList)

    val syntaxMember = "val %sSyntax = new scalaz.syntax.%sSyntax[F] {}".format(initLower(typeClassName), typeClassName)

    val mainSource = """package scalaz

trait %sLike[%s] %s { self =>
  ////

  // derived functions

  ////
  %s
}

////
/**
 *
 */
////
trait %s[%s] extends %sLike[F] {
  self %s =>

%s
}

object %s {
  def apply[%s](implicit F: %s[F]): %s[F] = F

  ////

  ////
}

trait %sInstance[%s] %s
""".format(typeClassName, classifiedType, extendsLikeList, syntaxMember, typeClassName, classifiedType,
      typeClassName,
      selfType,
      parentsDef,
      typeClassName, classifiedType, typeClassName, typeClassName,
      typeClassName, classifiedType, extendsInstanceList)
    val mainSourceFile = SourceFile(List("scalaz"), typeClassName + ".scala", mainSource)

    val syntaxSource = kind match {
      case Kind.* =>
        """package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `%s` */
trait %sV[F] extends SyntaxV[F] {
  ////

  ////
}

trait To%sSyntax %s {
  implicit def To%sV[F](v: F) =
    (new %sSyntax[F] {}).To%sV(v)

  ////

  ////
}

trait %sSyntax[F] %s {
  implicit def To%sV(v: F): %sV[F] = new %sV[F] { def self = v }

  ////

  ////
}
""".format(typeClassName, typeClassName, typeClassName, extendsToSyntaxListText,

      // implicits in ToXxxSyntax
      typeClassName, typeClassName, typeClassName,

      typeClassName, extendsListText("Syntax"),
      typeClassName, typeClassName, typeClassName
    )
    case Kind.*->* =>
    """package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `%s` */
trait %sV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait To%sSyntax %s {
  implicit def To%sV[F[_],A](v: F[A]) =
    (new %sSyntax[F] {}).To%sV(v)
  implicit def To%sVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new %sSyntax[({type f[a] = F[X, a]})#f] {}).To%sV(v)
  implicit def To%sVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new %sSyntax[({type f[a] = F[X, G, a]})#f] {}).To%sV(v)
  implicit def To%sVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new %sSyntax[({type f[a] = F[X, Id, a]})#f] {}).To%sV(v)

  ////

  ////
}

trait %sSyntax[F[_]] %s {
  implicit def To%sV[A](v: F[A]): %sV[F, A] = new %sV[F,A] { def self = v }

  ////

  ////
}
""".format(typeClassName, typeClassName, typeClassName, extendsToSyntaxListText,

          // implicits in ToXxxSyntax
          typeClassName, typeClassName, typeClassName,
          typeClassName, typeClassName, typeClassName,
          typeClassName, typeClassName, typeClassName,
          typeClassName, typeClassName, typeClassName,

          typeClassName, extendsListText("Syntax"),
          typeClassName, typeClassName, typeClassName
        )
    }
    val syntaxSourceFile = SourceFile(List("scalaz", "syntax"), typeClassName + "Syntax.scala", syntaxSource)
    TypeClassSource(mainSourceFile, syntaxSourceFile)
  }
}
