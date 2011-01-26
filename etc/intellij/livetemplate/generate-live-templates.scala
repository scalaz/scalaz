import collection.immutable.List
import collection.Seq
import java.lang.String
import xml.NodeSeq

case class Variable(name: String) {
  def toXml = <variable name={name} expression=" " defaultValue=" " alwaysStopAt="true"/>
}

case class Template(name: String, value: String, description: String, variables: List[Variable]) {
  def toXml =
  <template name={name} value={escape(value)} description={description} toReformat="false" toShortenFQNames="true">
    {for (v <- variables) yield v.toXml}
    <context>
      <option name="SCALA" value="true"/>
    </context>
  </template>
}

case class Param(val name: String)

def method(shortcut: String, name: String, params: Param*): List[Template] = {
  val varNames = params.map(_.name.toUpperCase).toList
  val varNamesDelimited = varNames.map("$" + _ + "$")
  val dotValue = "." + name + (if (params.isEmpty) "" else varNamesDelimited.mkString("(", ", ", ")"))
  val vars = varNames.map(Variable(_))
  val dotTemplate = Template("." + shortcut, dotValue, name, vars)
  val template = Template(shortcut, name + " " + varNamesDelimited.mkString(" "), name, vars)
  
  if (name.endsWith(":"))
    List(template)
  else if (params.size <= 1)
    List(dotTemplate, template)
  else
    List(dotTemplate)
}

def function(shortcut: String, name: String, args: Param*): List[Template] = {
  val varNames = args.map("$" + _.name.toUpperCase + "$")
  val vars = args.map((a: Param) => Variable(a.name.toUpperCase)).toList
  val template = Template(shortcut, name + (if (varNames.isEmpty) "" else varNames.mkString("(", ", ", ")")), name, vars)
  List(template)
}

def escape(xmlText: String): NodeSeq = {
  def escapeChar(c: Char): xml.Node =
    if (c > 0x7F || Character.isISOControl(c))
      xml.EntityRef("#" + Integer.toString(c, 10))
    else
      xml.Text(c.toString)

  new xml.Group(xmlText.map(escapeChar(_)))
}

val imports = Template("isz", "import scalaz._\nimport Scalaz._\n", "imports for Scalaz", List())

val templates = List(
  method("<x>", "<×>", Param("b")),
  method("<xx>", "<××>", Param("b"), Param("c")),
  method("<xxx>", "<×××>", Param("b"), Param("c"), Param("d")),
  method("<xxxx>", "<××××>", Param("b"), Param("c"), Param("d"), Param("d")),
  method("map", "∘", Param("f")),
  method("mapmap", "∘∘", Param("f")),
  method("o", "∘", Param("f")),
  method("oo", "∘∘", Param("f")),
  method("|->", "↦", Param("f")),
  method("bind", "∗", Param("f")),
  method("plus", "⊹", Param("a")),
  method("appendpure", "\u279C:", Param("a")),
  method("pluspure", "\u279D:", Param("a")),
  method("sum", "∑"),
  method("suml", "∑"),
  method("exists", "∃", Param("f")),
  method("forall", "∀", Param("f")),
  method("traversemonoid", "↣", Param("f")),
  method("join", "μ"),
  method("cojoin", "υ"),
  method("copure", "ε"),
  method("contramap", "∙", Param("f")),
  method("pure", "η"),
  function("kleisli", "☆", Param("f")),
  function("cokleisli", "★", Param("f")),
  method("dual", "σ"),
  method("equal", "≟", Param("a")),
  method("notequal", "≠", Param("a")),
  method("<=", "≤", Param("a")),
  method(">=", "≥", Param("a")),
  method("<", "≨", Param("a")),
  method(">>>", "⋙", Param("a")),
  method("<<<", "⋘", Param("a")),
  function("undefined", "⊥"),
  method("^", "∧", Param("a")),
  method("conjunction", "∧", Param("a")),
  method("disjunction", "∨", Param("a")),
  method("v", "∨", Param("a")),
  method("<==", "\u21D0", Param("a")),
  method("zipstream", "\u0290"),
  method("mult", "\u220f"),
  function("zero", "∅"),
  function("x", "×"),
  function("=>", "\u21D2"),
  method("3", "∋", Param("a")),
  method("contains", "∋", Param("a")),
  method("memberof", "∈:", Param("a"))
).flatten ++ List(imports)

val templateSet = <templateSet group="scalaz">
  {templates.map(_.toXml)}
</templateSet>

println(new xml.PrettyPrinter(120, 4).format(templateSet))
