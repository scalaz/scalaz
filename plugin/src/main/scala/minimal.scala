package scalaz
package plugin

import scala.tools.nsc._

abstract class SufficiencyChecker extends plugins.PluginComponent {
  val global: Global
  val scalazDefns: Definitions { val global: SufficiencyChecker.this.global.type }

  import global._, scalazDefns.{ global => _, _ }

  override val phaseName  = "scalaz-sufficiency"
  override val runsAfter  = "typer" :: Nil
  override val runsBefore = "refchecks" :: Nil

  object solver extends Solver[TermName]

  def newPhase(prev: Phase) = new GlobalPhase(prev) {
    def name = phaseName

    def apply(unit: CompilationUnit): Unit =
      CheckSufficiency.traverse(unit.body)
  }

  object CheckSufficiency extends Traverser {
    override def traverse(tree: Tree): Unit = {
      tree match {
        case cd @ ClassDef(_, _, _, _) =>
          val sym = cd.symbol
          if (sym.isClass && !sym.isAbstract) {
            val baseClasses = sym.baseClasses.tail // first base class is `sym`

            baseClasses
              .flatMap(b => minimalNames(b).map(b -> _))
              .foreach {
                case (b, namesAndPoss) =>
                  val namess = mmap(namesAndPoss)(_._1)
                  val implemented =
                    namess.flatten.flatMap { name =>
                      sym.info.member(name) match {
                        case NoSymbol =>
                          // we *should* have caught this!
                          devWarning(sym.pos, s"Base $b requires a method named $name but none exists!")
                          None
                        case found =>
                          if (found.owner eq b) None // not implemented if impl hails from its own owner
                          else Some(found.name.decodedName.toTermName)
                      }
                    }.toSet

                  solver.isFormulaSatisfied(implemented)(namess) match {
                    case None => // yep, we're good
                    case Some(unsat) =>
                      error.MissingImplementations(sym, unsat)
                  }
              }
          }

          if (sym.hasAnnotation(MinimalAttr)) {
            val Some(ann) = sym.getAnnotation(MinimalAttr)
            if (sym.isConcreteClass) {
              error.MinimalAnnotationOnNonTrait(sym, ann.pos)
            } else {
              val namess = minimalNames(ann)

              // must all exist
              mforeach(namess) {
                case (name, pos) =>
                  val member = sym.info.nonPrivateMember(name)
                  if (!member.exists) {
                    error.NotFound(name, pos)
                  } else if (member.isOverloaded) {
                    error.MinimalOverloaded(member)
                  } else if (!(member.isMethod && !member.isField)) {
                    error.MinimalNonMethod(member)
                  } else if (member.isDeferred) {
                    error.MinimalDeferred(member)
                  }
              }
            }
          }

        case md @ ModuleDef(_, _, _) =>
          val sym = md.symbol
          if (sym.hasAnnotation(MinimalAttr)) {
            val Some(ann) = sym.getAnnotation(MinimalAttr)
            error.MinimalAnnotationOnNonTrait(sym, ann.pos)
          }
        case _ =>
      }
      super.traverse(tree)
    }
  }

  object error {

    def MissingImplementations(sym: Symbol, formula: solver.Formula): Unit =
      globalError(sym.pos, s"missing implementation for methods: ${solver.showFormula(formula)}")
    def MinimalAnnotationOnNonTrait(sym: Symbol, pos: Position): Unit =
      globalError(pos, s"@${MinimalAttr.name} annotations may only be applied to traits")

    def NotFound(name: Name, pos: Position): Unit =
      globalError(pos, s"not found: $name")

    def MinimalOverloaded(sym: Symbol): Unit =
      globalError(sym.pos, s"methods making up a minimal definition must not be overloaded")

    def MinimalNonMethod(sym: Symbol): Unit =
      globalError(sym.pos, s"@${MinimalAttr.name} may only name methods, found $sym")

    def MinimalDeferred(sym: Symbol): Unit =
      globalError(sym.pos, s"methods making up a minimal definition must not be abstract")

  }

  def minimalNames(sym: Symbol): Option[List[List[(TermName, Position)]]] =
    sym.getAnnotation(MinimalAttr).map(minimalNames)

  def minimalNames(ann: AnnotationInfo): List[List[(TermName, Position)]] = {
    def isTupleApply(s: Symbol): Boolean =
      s.isMethod && definitions.isTupleSymbol(s.owner.companion)

    ann.tree match {
      case treeInfo.Applied(Select(New(_), _), _, args :: Nil) =>
        args.map {
          case a @ Literal(Constant(name: String)) =>
            (TermName(name), a.pos) :: Nil
          case Apply(fun, args) if isTupleApply(fun.symbol) =>
            args.map {
              case a @ Literal(Constant(name: String)) =>
                (TermName(name), a.pos)
            }
        }
    }
  }

}

class Solver[Literal] {
  type Clause  = List[Literal] // conjoined
  type Formula = List[Clause] // disjoined
  type Axioms  = Set[Literal]

  def isClauseSatisfied(axioms: Axioms)(clause: Clause): Option[Clause] =
    clause.filterNot(axioms.contains) match {
      case Nil   => None
      case unsat => Some(unsat)
    }

  def isFormulaSatisfied(axioms: Axioms)(formula: Formula): Option[Formula] = {
    val satisfictions = formula.map(isClauseSatisfied(axioms))
    if (satisfictions contains None) None
    else Some(satisfictions.flatten)
  }

  def showClause(clause: Clause): String = clause match {
    case only :: Nil => only.toString
    case _           => clause.mkString("(", " AND ", ")")
  }

  def showFormula(formula: Formula): String =
    formula.map(showClause).mkString(" OR ")
}
