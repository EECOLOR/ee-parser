package ee

sealed trait Definition {
  override def toString = getClass.getName
    .stripSuffix("$")
    .replaceAll("ee.", "")
    .replaceAll("Global\\$", "")
    .replaceAll("\\$", ".")
}

object Definition {
  implicit class Operations(definition:Definition) {
    def ~ (tail:Definition)  = new ~(definition, tail)
    def | (other:Definition) = new |(definition, other)

    def *       = AttributedDefinition(definition, zeroOrMore = true)
    def unary_! = AttributedDefinition(definition, not = true)
    def ?       = AttributedDefinition(definition, zeroOrOne = true)
    def +       = AttributedDefinition(definition, oneOrMore = true)
  }
}

trait Rule extends Definition {
  def := (definition:Definition)(implicit addRule: Rules.AddRule):Unit =
    addRule(this, definition)
}

sealed abstract class ToString(val name: String)       extends Definition { override def toString = name }
sealed abstract class Unspecified(name:String)         extends ToString(name)

sealed trait CompoundDefinition extends Definition {
  def left: Definition
  def right: Definition
}
case class ~(left: Definition, right: Definition) extends ToString(s"($left ~ $right)")  with CompoundDefinition
case class |(left: Definition, right: Definition) extends ToString(s"($left | $right)") with CompoundDefinition

case class AttributedDefinition(
  description: Definition,

  zeroOrMore : Boolean = false,
  not : Boolean = false,
  zeroOrOne : Boolean = false,
  oneOrMore: Boolean = false
) extends Definition {

  private val $ = Map(
    '!' -> not,
    '*' -> zeroOrMore,
    '?' -> zeroOrOne,
    '+' -> oneOrMore
  ) map { 
    case (char, true) => char -> char.toString
    case (char, _)    => char -> ""
  }

  override def toString = $('!') + description + $('*') + $('?') + $('+') 
}

case object Id             extends Unspecified("`Id`")
case object Literal        extends Unspecified("`Literal`")
case object PrefixOperator extends Unspecified("`PrefixOperator`")

// make aliases for these
case object `_`   extends Unspecified("`_`")
case object `.`   extends Unspecified("`.`")
case object `,`   extends Unspecified("`,`")
case object `:`   extends Unspecified("`:`")
case object `=`   extends Unspecified("`=`")
case object `|`   extends Unspecified("`|`")
case object `@`   extends Unspecified("`@`")
case object `#`   extends Unspecified("`#`")
case object `*`   extends Unspecified("`*`")
case object `S`   extends Unspecified("`$`")
case object `"`   extends Unspecified("`\"`")
case object `{`   extends Unspecified("`{`")
case object `}`   extends Unspecified("`}`")
case object `)`   extends Unspecified("`)`")
case object `(`   extends Unspecified("`(`")
case object `]`   extends Unspecified("`]`")
case object `[`   extends Unspecified("`[`")
case object `=>`  extends Unspecified("`=>`")
case object `<-`  extends Unspecified("`<-`")
case object `'''` extends Unspecified("`'''`")
case object `<:`  extends Unspecified("`<:`")
case object `>:`  extends Unspecified("`>:`")
case object `<%`  extends Unspecified("`<%`")

case object `import`  extends Unspecified("`import`")
case object `package` extends Unspecified("`package`")
case object `match`   extends Unspecified("`match`")
case object `case`    extends Unspecified("`case`")
case object `if`      extends Unspecified("`if`")
case object `return`  extends Unspecified("`return`")
case object `new`     extends Unspecified("`new`")
case object `with`    extends Unspecified("`with`")
case object `throw`   extends Unspecified("`throw`")
case object `for`     extends Unspecified("`for`")
case object `do`      extends Unspecified("`do`")
case object `while`   extends Unspecified("`while`")
case object `try`     extends Unspecified("`try`")
case object `catch`   extends Unspecified("`catch`")
case object `finally` extends Unspecified("`finally`")
case object `else`    extends Unspecified("`else`")
case object `yield`   extends Unspecified("`yield`")
case object `macro`   extends Unspecified("`macro`")
case object `extends` extends Unspecified("`extends`")

case object `public`    extends Unspecified("`public`")
case object `private`   extends Unspecified("`private`")
case object `protected` extends Unspecified("`protected`")

case object `implicit` extends Unspecified("`implicit`")
case object `abstract` extends Unspecified("`abstract`")
case object `override` extends Unspecified("`override`")

case object `val`  extends Unspecified("`val`")
case object `var`  extends Unspecified("`var`")
case object `def`  extends Unspecified("`def`")
case object `type` extends Unspecified("`type`")

case object `covariant`     extends Unspecified("`covariant`")
case object `contravariant` extends Unspecified("`contravariant`")

case object `trait` extends Unspecified("`trait`")
case object `object` extends Unspecified("`object`")
case object `class` extends Unspecified("`class`")
