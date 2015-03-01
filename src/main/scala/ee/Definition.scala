package ee

sealed trait Definition {
  def *       = AttributedDescription(this, zeroOrMore = true)
  def unary_! = AttributedDescription(this, not = true)
  def ?       = AttributedDescription(this, zeroOrOne = true)
  def +       = AttributedDescription(this, oneOrMore = true)

  override def toString = getClass.getSimpleName.stripSuffix("$")
}

object Definition {
  implicit class DecorateDefinition(definition:Definition) {
    def ~ (tail:Definition)  = new ~(definition, tail)
    def | (other:Definition) = new |(definition, other)
  }
}

abstract class Description(definitions: => Definition) extends Definition { lazy val contents = definitions }
sealed abstract class ToString(val name: String)       extends Definition { override def toString = name }
sealed abstract class Unspecified(name:String)         extends ToString(name)

case class ~(description:Definition, rest:Definition)  extends ToString(s"($description ~ $rest)")
case class |(description:Definition, other:Definition) extends ToString(s"($description | $other)")

case class AttributedDescription(
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
case object `$`   extends Unspecified("`$`")
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
case object `<:` extends Unspecified("`<:`")
case object `>:` extends Unspecified("`>:`")
case object `<%` extends Unspecified("`<%`")

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
