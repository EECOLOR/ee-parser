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

sealed trait Unspecified                               extends Definition
abstract class Description(val contents:Definition)    extends Definition
sealed abstract class ToString(string: String)         extends Definition { override def toString = string }

case class ~(description:Definition, rest:Definition)  extends ToString(description + " ~ " + rest)
case class |(description:Definition, other:Definition) extends ToString(description + " | " + other)

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

case object Id       extends Unspecified
case object Literals extends Unspecified

case object `_`  extends Unspecified
case object `.`  extends Unspecified
case object `,`  extends Unspecified
case object `:`  extends Unspecified
case object `=`  extends Unspecified
case object `|`  extends Unspecified
case object `@`  extends Unspecified
case object `#`  extends Unspecified
case object `*`  extends Unspecified
case object `$`  extends Unspecified
case object `"`  extends Unspecified
case object `{`  extends Unspecified
case object `}`  extends Unspecified
case object `)`  extends Unspecified
case object `(`  extends Unspecified
case object `]`  extends Unspecified
case object `[`  extends Unspecified
case object `=>` extends Unspecified

case object `import`  extends Unspecified
case object `package` extends Unspecified
case object `match`   extends Unspecified
case object `case`    extends Unspecified
case object `if`      extends Unspecified