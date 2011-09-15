import sbt._


object GenTypeClass {
  case class SourceFile(packages: List[String], fileName: String, source: String) {
    def file(scalaSource: File): File = packages.foldLeft(scalaSource)((file, p) => file / p) / fileName
  }
  case class TypeClassSource(mainFile: SourceFile, syntaxFile: SourceFile) {
    def sources = List(mainFile, syntaxFile)
  }

  def typeclassSource(typeClassName: String, extendsList: Seq[String]): TypeClassSource = {
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
    val mainSource = """package scalaz

trait %sLike[F[_]] %s { self =>

  // derived functions


  override val syntax = new scalaz.syntax.%sSyntax[F] {}
}
trait %s[F[_]] extends %sLike[F]
trait %sInstance[F[_]] %s
    """.format(typeClassName, extendsLikeList, typeClassName, typeClassName, typeClassName, typeClassName, extendsInstanceList)
    val mainSourceFile = SourceFile(List("scalaz"), typeClassName + ".scala", mainSource)

    val syntaxSource = """package scalaz
package syntax

import Id.Id

/** Wraps a value `self` and provides methods related to `%s` */
trait %sV[F[_],A] extends SyntaxV[F[A]] {
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
}

trait %sSyntax[F[_]] %s {
  implicit def %sV[A](v: F[A]): %sV[F, A] = new %sV[F,A] { def self = v }
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
    val syntaxSourceFile = SourceFile(List("scalaz", "syntax"), typeClassName + "Syntax.scala", syntaxSource)
    TypeClassSource(mainSourceFile, syntaxSourceFile)
  }
}
