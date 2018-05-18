package scalaz
package meta

import language.experimental.macros
import reflect.macros.blackbox

@com.github.ghik.silencer.silent
final class minimal(methods: Any*) extends annotation.StaticAnnotation

final class IsMinimal //private ()

object IsMinimal {
  def apply(): IsMinimal = macro proveMinimality_impl

  def proveMinimality_impl(c: blackbox.Context)(): c.Tree = {
    import c.universe._, c.internal._

    def minimalNames(sym: Symbol): Option[List[List[TermName]]] = {
      def isTupleApply(s: Symbol): Boolean = {
        s.isMethod && definitions.TupleClass.seq.contains(s.owner.companion)
      }

      sym.annotations.find(_.tree.tpe.typeSymbol eq symbolOf[minimal]).map { ann =>
        ann.tree match {
          case Apply(Select(New(_), _), argss) =>
            argss.map {
              case Literal(Constant(name: String)) => TermName(name) :: Nil
              case Apply(fun, args) if isTupleApply(fun.symbol) =>
                args.map {
                  case Literal(Constant(name: String)) => TermName(name)
                }
            }
        }
      }
    }
    object solver extends Solver[TermName]

    val enclClass = Iterator
      .iterate(enclosingOwner)(_.owner)
      .takeWhile(_ ne NoSymbol)
      .collectFirst { case cls: ClassSymbol => cls }
      .getOrElse {
        c.abort(c.enclosingPosition, s"proveMinimality not called inside a class!")
      }

    if (enclClass.isAbstract) // includes traits
      c.error(c.enclosingPosition, s"IsMinimal may only be implemented in classes")

    val bases = enclClass.baseClasses.tail

    bases.flatMap(b => minimalNames(b).map(b -> _)).foreach {
      case (b, namess) =>
        val implemented =
          namess.flatten.flatMap { name =>
            enclClass.info.member(name) match {
              case NoSymbol => ???
              case found =>
                if (found.owner eq b) None // not implemented if impl hails from its own owner
                else Some(found.name.decodedName.toTermName)
            }
          }.toSet
        solver.isFormulaSatisfied(implemented)(namess) match {
          case None => // yep, we're good
          case Some(unsat) =>
            c.error(enclClass.pos, s"missing implementations: ${solver.showFormula(unsat)}")
        }
    }

    Literal(Constant(null))
  }
}

private[meta] class Solver[Literal] {
  type Clause = List[Literal] // conjoined
  type Formula = List[Clause] // disjoined
  type Axioms = Set[Literal]

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

  def showClause(clause: Clause): String =
    clause.mkString("(", " AND ", ")")
  def showFormula(formula: Formula): String =
    formula.map(showClause).mkString(" OR ")

}