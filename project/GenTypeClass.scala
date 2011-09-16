import sbt._


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
    def unapply(s: String): Option[Kind] = GenTypeClass.Kind.parse(s)
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
    def extendsListText(suffix: String) = extendsList match {
      case Seq() => ""
      case es => es.map(n => n + suffix + "[F]").mkString("extends ", " with ", "")
    }
    def extendsToSyntaxListText = extendsList match {
      case Seq() => ""
      case es => es.map(n => "To" + n + "Syntax").mkString("extends ", " with ", "")
    }
    val extendsLikeList = extendsListText("Like")
    val extendsInstanceList = extendsListText("Instance")

    val syntaxOverride =
      if (extendsList.size > 1) ""
      else {
        val modifier = if (extendsList.isEmpty) "" else "override "
        modifier + "val syntax = new scalaz.syntax.%sSyntax[F] {}".format(typeClassName)
      }

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
trait %s[%s] extends %sLike[F]

trait %sInstance[%s] %s
""".format(typeClassName, classifiedType, extendsLikeList, syntaxOverride, typeClassName, classifiedType,
      typeClassName, typeClassName, classifiedType, extendsInstanceList)
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
  implicit def %s[F](v: F) =
    (new %sSyntax[F] {}).%sV(v)

  ////

  ////
}

trait %sSyntax[F] %s {
  implicit def %sV(v: F): %sV[F] = new %sV[F] { def self = v }

  ////

  ////
}
""".format(typeClassName, typeClassName, typeClassName, extendsToSyntaxListText,

      // implicits in ToXxxSyntax
      initLower(typeClassName), typeClassName, initLower(typeClassName),

      typeClassName, extendsListText("Syntax"), initLower(typeClassName),
      typeClassName, typeClassName
    )
    case Kind.*->* =>
    """package scalaz
package syntax

import Id.Id

/** Wraps a value `self` and provides methods related to `%s` */
trait %sV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait To%sSyntax %s {
  implicit def %s[F[_],A](v: F[A]) =
    (new %sSyntax[F] {}).%sV(v)
  implicit def %sBin[F[_, _], X, A](v: F[X, A]) =
    (new %sSyntax[({type f[a] = F[X, a]})#f] {}).%sV(v)
  implicit def %sBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new %sSyntax[({type f[a] = F[X, G, a]})#f] {}).%sV(v)
  implicit def %sBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new %sSyntax[({type f[a] = F[X, Id, a]})#f] {}).%sV(v)

  ////

  ////
}

trait %sSyntax[F[_]] %s {
  implicit def %sV[A](v: F[A]): %sV[F, A] = new %sV[F,A] { def self = v }

  ////

  ////
}
""".format(typeClassName, typeClassName, typeClassName, extendsToSyntaxListText,

          // implicits in ToXxxSyntax
          initLower(typeClassName), typeClassName, initLower(typeClassName),
          initLower(typeClassName), typeClassName, initLower(typeClassName),
          initLower(typeClassName), typeClassName, initLower(typeClassName),
          initLower(typeClassName), typeClassName, initLower(typeClassName),

          typeClassName, extendsListText("Syntax"), initLower(typeClassName),
          typeClassName, typeClassName
        )
    }
    val syntaxSourceFile = SourceFile(List("scalaz", "syntax"), typeClassName + "Syntax.scala", syntaxSource)
    TypeClassSource(mainSourceFile, syntaxSourceFile)
  }
}
