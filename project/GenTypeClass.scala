import sbt._

case class TypeClass(name: String, kind: Kind, pack: Seq[String] = Seq("scalaz"), extendsList: Seq[TypeClass] = Seq(), createSyntax: Boolean = true) {
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
  lazy val equal = TypeClass("Equal", *)
  lazy val show = TypeClass("Show", *)
  lazy val order = TypeClass("Order", *, extendsList = Seq(equal))
  lazy val enum = TypeClass("Enum", *, extendsList = Seq(order))

  lazy val invariantFunctor = TypeClass("InvariantFunctor", *->*)
  lazy val functor = TypeClass("Functor", *->*, extendsList = Seq(invariantFunctor))
  lazy val apply: TypeClass = TypeClass("Apply", *->*, extendsList = Seq(functor))
  lazy val applicative = TypeClass("Applicative", *->*, extendsList = Seq(apply))
  lazy val align = TypeClass("Align", *->*, extendsList = Seq(functor))
  lazy val zip = TypeClass("Zip", *->*)
  lazy val unzip = TypeClass("Unzip", *->*)
  lazy val bind = TypeClass("Bind", *->*, extendsList = Seq(apply))
  lazy val monad = TypeClass("Monad", *->*, extendsList = Seq(applicative, bind))
  lazy val foldable = TypeClass("Foldable", *->*)
  lazy val foldable1 = TypeClass("Foldable1", *->*, extendsList = Seq(foldable))
  lazy val traverse = TypeClass("Traverse", *->*, extendsList = Seq(functor, foldable))
  lazy val traverse1 = TypeClass("Traverse1", *->*, extendsList = Seq(traverse, foldable1))

  lazy val contravariant = TypeClass("Contravariant", *->*, extendsList = Seq(invariantFunctor))
  lazy val divide = TypeClass("Divide", *->*, extendsList = Seq(contravariant))
  lazy val divisible = TypeClass("Divisible", *->*, extendsList = Seq(divide))
  lazy val cobind = TypeClass("Cobind", *->*, extendsList = Seq(functor))
  lazy val comonad = TypeClass("Comonad", *->*, extendsList = Seq(cobind))
  lazy val cozip = TypeClass("Cozip", *->*)

  lazy val plus = TypeClass("Plus", *->*, extendsList = Seq())
  lazy val plusEmpty = TypeClass("PlusEmpty", *->*, extendsList = Seq(plus))
  lazy val isEmpty = TypeClass("IsEmpty", *->*, extendsList = Seq(plusEmpty))
  lazy val optional = TypeClass("Optional", *->*)

  lazy val applicativePlus = TypeClass("ApplicativePlus", *->*, extendsList = Seq(applicative, plusEmpty))
  lazy val monadPlus = TypeClass("MonadPlus", *->*, extendsList = Seq(monad, applicativePlus))

  lazy val associative = TypeClass("Associative", *^*->*)
  lazy val bifunctor = TypeClass("Bifunctor", *^*->*)
  lazy val bifoldable = TypeClass("Bifoldable", *^*->*)
  lazy val bitraverse = TypeClass("Bitraverse", *^*->*, extendsList = Seq(bifunctor, bifoldable))
  lazy val compose = TypeClass("Compose", *^*->*)
  lazy val catchable = TypeClass("Catchable", *->*, extendsList = Seq())
  lazy val nondeterminism = TypeClass("Nondeterminism", *->*, extendsList = Seq(monad))
  lazy val category = TypeClass("Category", *^*->*, extendsList = Seq(compose))
  lazy val choice = TypeClass("Choice", *^*->*, extendsList = Seq(category))
  lazy val split = TypeClass("Split", *^*->*, extendsList = Seq(compose))
  lazy val profunctor = TypeClass("Profunctor", *^*->*, extendsList = Seq())
  lazy val strong = TypeClass("Strong", *^*->*, extendsList = Seq(profunctor))
  lazy val proChoice = TypeClass("ProChoice", *^*->*, extendsList = Seq(profunctor))
  lazy val arrow = TypeClass("Arrow", *^*->*, extendsList = Seq(split, strong, category))

  lazy val liftIO = TypeClass("LiftIO", *->*, pack = Seq("scalaz", "effect"))
  lazy val monadIO = TypeClass("MonadIO", *->*, extendsList = Seq(liftIO, monad), pack = Seq("scalaz", "effect"))
  lazy val liftControlIO = TypeClass("LiftControlIO", *->*, pack = Seq("scalaz", "effect"))
  lazy val monadControlIO = TypeClass("MonadControlIO", *->*, extendsList = Seq(liftControlIO, monad), pack = Seq("scalaz", "effect"))
  lazy val resource = TypeClass("Resource", *, pack = Seq("scalaz", "effect"))

  lazy val monadState = TypeClass("MonadState", |*->*|->*, extendsList = Seq(monad), createSyntax = false)
  lazy val monadError = TypeClass("MonadError", |*->*|->*, extendsList = Seq(monad))
  lazy val monadTell = TypeClass("MonadTell", |*->*|->*, extendsList = Seq(monad))
  lazy val monadReader = TypeClass("MonadReader", |*->*|->*, extendsList = Seq(monad), createSyntax = false)
  lazy val comonadStore = TypeClass("ComonadStore", |*->*|->*, extendsList = Seq(comonad), createSyntax = false)

  lazy val bindRec = TypeClass("BindRec", *->*, extendsList = Seq(bind))

  def core: List[TypeClass] = List(semigroup,
    monoid,
    equal,
    show,
    order,
    enum,
    plusEmpty,
    isEmpty,
    optional,
    invariantFunctor,
    functor,
    contravariant,
    divide,
    divisible,
    apply,
    applicative,
    align,
    zip,
    unzip,
    cozip,
    bind,
    monad,
    cobind,
    comonad,
    plus,
    applicativePlus,
    monadPlus,
    foldable,
    foldable1,
    traverse,
    traverse1,
    associative,
    bifunctor,
    bifoldable,
    bitraverse,
    catchable,
    nondeterminism,
    compose,
    category,
    choice,
    split,
    profunctor,
    strong,
    proChoice,
    arrow,
    monadState,
    monadError,
    monadTell,
    monadReader,
    comonadStore,
    bindRec
  )
  lazy val concurrent = Seq[TypeClass]()
  def effect = Seq(liftIO, monadIO, liftControlIO, monadControlIO, resource)
}

sealed abstract class Kind(val multipleParam: Boolean)

object Kind {

  case object * extends Kind(false)

  case object *->* extends Kind(false)

  case object *^*->* extends Kind(false)

  case object |*->*|->* extends Kind(true)
}

sealed trait FileStatus

object FileStatus{
  case object NoChange extends FileStatus
  case object Updated  extends FileStatus
  case object Created  extends FileStatus
}

object GenTypeClass {
  val useDependentMethodTypes = true

  case class SourceFile(packages: Seq[String], fileName: String, source: String) {
    def file(scalaSource: File): File = packages.foldLeft(scalaSource)((file, p) => file / p) / fileName

    def createOrUpdate(scalaSource: File, log: Logger): (FileStatus, sbt.File) = {
      val f = file(scalaSource)
      val (status, updatedSource) = if (f.exists()) {
        val old = IO.read(f)
        val updated = updateSource(old)
        if(updated == old){
          log.debug("No changed %s".format(f))
          (FileStatus.NoChange, updated)
        }else{
          log.info("Updating %s".format(f))
          (FileStatus.Updated, updated)
        }
      } else {
        log.info("Creating %s".format(f))
        (FileStatus.Created, source)
      }
      log.debug("Contents: %s".format(updatedSource))
      IO.delete(f)
      IO.write(f, updatedSource)
      (status, f)
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

  case class TypeClassSource(mainFile: SourceFile, syntaxFile: Option[SourceFile]) {
    def sources: List[SourceFile] = mainFile :: syntaxFile.toList
  }

  def typeclassSource(tc: TypeClass): TypeClassSource = {
    val typeClassName = tc.name
    val kind = tc.kind
    val extendsList = tc.extendsList.toList.map(_.name)

    import TypeClass._
    val classifiedTypeIdent = if (Set(arrow, associative, category, choice, split, compose, profunctor, strong, proChoice)(tc)) "=>:"
    else "F"

    val typeShape: String = kind match {
      case Kind.*      => ""
      case Kind.*->*   => "[_]"
      case Kind.*^*->* => "[_, _]"
      case Kind.|*->*|->* => "[_], S"
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
    val syntaxMember = if(tc.createSyntax) {
      if (kind.multipleParam) {
        s"  val ${Util.initLower(typeClassName)}Syntax = new $syntaxPackString1.${typeClassName}Syntax[$classifiedTypeIdent, S] { def F = $typeClassName.this }"
      } else {
        s"  val ${Util.initLower(typeClassName)}Syntax = new $syntaxPackString1.${typeClassName}Syntax[$classifiedTypeIdent] { def F = $typeClassName.this }"
      }
    } else ""

    val applyMethod = if(kind.multipleParam) {
      s"""@inline def apply[$classifiedTypeF](implicit F: $typeClassName[F, S]): $typeClassName[F, S] = F"""
    } else {
      s"""@inline def apply[$classifiedTypeF](implicit F: $typeClassName[F]): $typeClassName[F] = F"""
    }

    val mainSource = s"""${tc.packageString0}

////
/**
 *
 */
////
trait $typeClassName[$classifiedType] $extendsLikeList { self =>
  ////

  ////
$syntaxMember
}

object $typeClassName {
  $applyMethod

  ////

  ////
}
"""
    val mainSourceFile = SourceFile(tc.pack, typeClassName + ".scala", mainSource)

    val syntaxSource = kind match {
      case Kind.* =>
        s"""$syntaxPackString

/** Wraps a value `self` and provides methods related to `${typeClassName}` */
final class ${typeClassName}Ops[F] private[syntax](val self: F)(implicit val F: ${typeClassName}[F]) extends Ops[F] {
  ////

  ////
}

trait To${typeClassName}Ops $extendsToSyntaxListText {
  implicit def To${typeClassName}Ops[F](v: F)(implicit F0: ${typeClassName}[F]) =
    new ${typeClassName}Ops[F](v)

  ////

  ////
}

trait ${typeClassName}Syntax[F] ${extendsListText("Syntax", cti = "F")} {
  implicit def To${typeClassName}Ops(v: F): ${typeClassName}Ops[F] = new ${typeClassName}Ops[F](v)(${typeClassName}Syntax.this.F)

  def F: ${typeClassName}[F]
  ////

  ////
}
"""
    case Kind.*->* =>
      val ToVUnapply =
s"""  implicit def To${typeClassName}OpsUnapply[FA](v: FA)(implicit F0: Unapply[${typeClassName}, FA]) =
    new ${typeClassName}Ops[F0.M,F0.A](F0(v))(F0.TC)
"""
      val ToVMA =
s"""  implicit def To${typeClassName}Ops[F[_],A](v: F[A])(implicit F0: ${typeClassName}[F]) =
    new ${typeClassName}Ops[F,A](v)
"""

    s"""$syntaxPackString

/** Wraps a value `self` and provides methods related to `${typeClassName}` */
final class ${typeClassName}Ops[F[_],A] private[syntax](val self: F[A])(implicit val F: ${typeClassName}[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait To${typeClassName}Ops0 {
$ToVUnapply
}

trait To${typeClassName}Ops $extendsToSyntaxListText {
$ToVMA
  ////

  ////
}

trait ${typeClassName}Syntax[F[_]] ${extendsListText("Syntax", cti = "F")} {
  implicit def To${typeClassName}Ops[A](v: F[A]): ${typeClassName}Ops[F, A] = new ${typeClassName}Ops[F,A](v)(${typeClassName}Syntax.this.F)

  def F: ${typeClassName}[F]
  ////

  ////
}
"""
      case Kind.*^*->* =>

        val ToVUnapply =
  s"""  implicit def To${typeClassName}OpsUnapply[FA](v: FA)(implicit F0: Unapply2[${typeClassName}, FA]) =
    new ${typeClassName}Ops[F0.M,F0.A,F0.B](F0(v))(F0.TC)
"""
        val ToVKleisli =
  s"""implicit def To${typeClassName}VFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: ${typeClassName}[F[G, ?, ?]]) =
    new ${typeClassName}Ops[F[G, ?, ?], A, B](v)(F0)
"""
       val ToVFAB =
  s"""implicit def To${typeClassName}Ops[F[_, _],A, B](v: F[A, B])(implicit F0: ${typeClassName}[F]) =
    new ${typeClassName}Ops[F,A, B](v)
"""


    s"""$syntaxPackString

/** Wraps a value `self` and provides methods related to `${typeClassName}` */
final class ${typeClassName}Ops[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: ${typeClassName}[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait To${typeClassName}Ops0 {
$ToVUnapply
}

trait To${typeClassName}Ops ${extendsToSyntaxListText} {

  $ToVFAB

  $ToVKleisli
  ////

  ////
}

trait ${typeClassName}Syntax[F[_, _]] ${extendsListText("Syntax", cti = "F")} {
  implicit def To${typeClassName}Ops[A, B](v: F[A, B]): ${typeClassName}Ops[F, A, B] = new ${typeClassName}Ops[F, A, B](v)(${typeClassName}Syntax.this.F)

  def F: ${typeClassName}[F]
  ////

  ////
}
"""
      case Kind.|*->*|->* =>

        val ToOps =
  s"""implicit def To${typeClassName}Ops[F[_], S, A](v: F[A])(implicit F0: ${typeClassName}[F, S]) =
    new ${typeClassName}Ops[F, S, A](v)"""


    s"""$syntaxPackString

/** Wraps a value `self` and provides methods related to `${typeClassName}` */
final class ${typeClassName}Ops[F[_], S, A] private[syntax](self: F[A])(implicit val F: ${typeClassName}[F, S]) {
  ////

  ////
}

trait To${typeClassName}Ops ${extendsToSyntaxListText} {
  $ToOps

  ////

  ////
}

trait ${typeClassName}Syntax[F[_], S] ${extendsListText("Syntax", cti = "F")} {
  implicit def To${typeClassName}Ops[A](v: F[A]): ${typeClassName}Ops[F, S, A] =
    new ${typeClassName}Ops[F, S, A](v)(${typeClassName}Syntax.this.F)

  def F: ${typeClassName}[F, S]
  ////

  ////
}
"""
    }
    val syntaxSourceFile = if(tc.createSyntax){
      Some(SourceFile(tc.syntaxPack, typeClassName + "Syntax.scala", syntaxSource))
    } else None

    TypeClassSource(mainSourceFile, syntaxSourceFile)
  }
}
