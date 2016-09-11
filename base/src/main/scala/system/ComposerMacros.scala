package scalaz
package system

import scala.reflect.macros.whitebox.Context

// Originally inspired by https://github.com/scala/scala/tree/7b890f71ecd0d28c1a1b81b7abfe8e0c11bfeb71/test/files/run/t5923c

object ComposerMacros {
  import Composer.{formatError, Ops}

  val composers: List[Composer] = List(ComposerFunctions)

  def unpack(c: Context)(tpe: c.Type): Either[String, Vector[c.Type]] = {
    import c.universe._
    val sym = tpe.typeSymbol

    if (!(tpe <:< typeOf[Product]) && !definitions.TupleClass.seq.contains(sym))
      Left(s"$sym is not a tuple")
    else {
      val TypeRef(_, _, ts) = tpe
      if (ts.size < 2) Left(s"can't compose less than two components")
      else Right(ts.toVector)
    }
  }

  def materializeOps[XS: c.WeakTypeTag, O: c.WeakTypeTag](c: Context): c.Expr[Ops[XS, O]] = {
    import c.universe._

    val tpe = c.weakTypeOf[XS]
    unpack(c)(tpe) match {
      case Left(err) =>
        // We abort the implicit resolution, giving a chance for others to resolve.
        c.abort(c.enclosingPosition, err)
      case Right(ts) =>
        val x = ts(0)
        val y = ts(1)
        val o = composers.foldLeft(None: Option[Type])  {
          case (Some(a),  p) => Some(a)
          case (_,        p) => (1 until ts.size).foldLeft(Some(ts(0)): Option[Type]) {
            case (Some(u), i) => p.typecheck(c)(u, ts(i)).fold[Option[Type]](_ => None, Some(_))
            case (None, _)    => None
          }
        } match {
          case Some(tpe)  =>
            // We it does typecheck, we return the resulting type of the composition.
            tpe
          case None       =>
            // We return Null in case of typechecking failure,
            // it will provide a nice error by failing at the next phase.
            typeOf[Null]
        }
        c.Expr[Ops[XS, O]](q"new scalaz.system.Composer.Ops[$tpe, $o]")
    }
  }

  def composeOps[XS: c.WeakTypeTag, O: c.WeakTypeTag](c: Context): c.Expr[O] = {
    import c.universe._
    val Apply(Apply(_, List(tree)), _) = c.prefix.tree

    val (xs, o) = (c.weakTypeOf[XS], c.weakTypeOf[O])
    val TypeRef(_, _, ts0) = xs
    val ts = ts0.toVector

    tree match {
      case Apply(_, ys) =>
        val vs = ys.toVector
        if (o =:= typeOf[Null]) {
          // Typechecking failed at previous phase, we run it again and track values to return nice error messages.
          val x = ts(0)
          val y = ts(1)
          val fail = Left((c.enclosingPosition, formatError(x.toString, y.toString))): Either[(Position, String), c.Type]
          composers.foldLeft(fail)  {
            case (Left(_),  p) =>
              p.typecheck(c)(x, y) match {
                case Left(error)  => Left((vs(1).pos, error))
                case Right(b)     =>
                  type State = Either[(Int, String), c.Type]
                  (2 until ts.size).foldLeft(Right(b): State) {
                    case (Right(u),   i) => p.typecheck(c)(u, ts(i)).fold[State](error => Left((i, error)), Right(_))
                    case (Left(err),  i) => Left(err)
                  } match {
                    case Right(o)       => Right(o)
                    case Left((i, msg)) => Left((vs(i).pos, msg))
                  }
              }
            case (right,    _) => right
          } match {
            case Left((p, err)) =>
              c.abort(p, err)
            case Right(expr) =>
              c.abort(c.enclosingPosition, s"Composer error: was expecting typechecking error")
          }
        } else {
          // All good, we generate and return the composite.
          val o = weakTypeOf[O]
          composers.find(_.isComposite(c)(o)) match {
            case Some(composer) =>
              composer.apply[O](c)(ts, vs)
            case None =>
              c.abort(c.enclosingPosition, s"Composer error: was expecting valid composite")
          }
        }
      case _ =>
        c.abort(c.enclosingPosition, s"can't statically compose a value")
    }
  }
}
