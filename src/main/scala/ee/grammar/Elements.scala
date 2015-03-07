package ee.grammar

sealed trait Element

object Element {
  implicit class Operations(element:Element) {

    def ~ (tail:Element):Sequence = new ~(element, tail)
    def | (other:Element):Choice  = new |(element, other)

    def *       : AttributedElement = AttributedElement(element, zeroOrMore = true)
    def ?       : AttributedElement = AttributedElement(element, zeroOrOne = true)
    def +       : AttributedElement = AttributedElement(element, oneOrMore = true)
    def unary_! : AttributedElement = AttributedElement(element, not = true)
  }
}

case class Sequence(head: Element, tail : Element) extends Element
case class Choice  (left: Element, right: Element) extends Element

case class AttributedElement(
  element    : Element,

  zeroOrMore : Boolean = false,
  zeroOrOne  : Boolean = false,
  oneOrMore  : Boolean = false,
  not        : Boolean = false
) extends Element

// we can lose this once we have union types
sealed trait `Nonterminal | Terminal` extends Element

sealed trait Terminal extends `Nonterminal | Terminal`


object Terminal {
  case object Id             extends Terminal
  case object Number         extends Terminal
  case object Symbol         extends Terminal
  case object PrefixOperator extends Terminal

  // make aliases for these
  case object `__`  extends Terminal
  case object `.`   extends Terminal
  case object `,`   extends Terminal
  case object `:`   extends Terminal
  case object `=`   extends Terminal
  case object `|`   extends Terminal
  case object `@`   extends Terminal
  case object `#`   extends Terminal
  case object `*`   extends Terminal
  case object `S`   extends Terminal
  case object `"`   extends Terminal
  case object `{`   extends Terminal
  case object `}`   extends Terminal
  case object `)`   extends Terminal
  case object `(`   extends Terminal
  case object `]`   extends Terminal
  case object `[`   extends Terminal
  case object `=>`  extends Terminal
  case object `<-`  extends Terminal
  case object `'''` extends Terminal
  case object `<:`  extends Terminal
  case object `>:`  extends Terminal
  case object `<%`  extends Terminal

  case object `import`  extends Terminal
  case object `package` extends Terminal
  case object `match`   extends Terminal
  case object `case`    extends Terminal
  case object `if`      extends Terminal
  case object `return`  extends Terminal
  case object `new`     extends Terminal
  case object `with`    extends Terminal
  case object `throw`   extends Terminal
  case object `for`     extends Terminal
  case object `do`      extends Terminal
  case object `while`   extends Terminal
  case object `try`     extends Terminal
  case object `catch`   extends Terminal
  case object `finally` extends Terminal
  case object `else`    extends Terminal
  case object `yield`   extends Terminal
  case object `macro`   extends Terminal
  case object `extends` extends Terminal
  // object A; object B; case class Pair[A, B](a: A, b: B)
  // val x:Pair[T, T] forSome { type T} = Pair(A, B)
  case object `forSome` extends Terminal

  case object `public`    extends Terminal
  case object `private`   extends Terminal
  case object `protected` extends Terminal

  case object `implicit` extends Terminal
  case object `abstract` extends Terminal
  case object `override` extends Terminal

  case object `val`  extends Terminal
  case object `var`  extends Terminal
  case object `def`  extends Terminal
  case object `type` extends Terminal

  case object `covariant`     extends Terminal
  case object `contravariant` extends Terminal

  case object `trait`  extends Terminal
  case object `object` extends Terminal
  case object `class`  extends Terminal
  case object `enum`   extends Terminal

  case object `true`      extends Terminal
  case object `false`     extends Terminal
  case object `null`      extends Terminal

  case object `sync`      extends Terminal
  case object `volatile`  extends Terminal
  case object `transient` extends Terminal
  case object `native`    extends Terminal
  case object `lazy`      extends Terminal
  case object `sealed`    extends Terminal
  case object `final`     extends Terminal
  case object `static`    extends Terminal

  case object `generateEquals`  extends Terminal
  case object `generateUnapply` extends Terminal
  case object `generateApply`   extends Terminal
}

sealed trait Nonterminal extends `Nonterminal | Terminal` {
  def := (element:Element)(implicit addProduction: Productions.AddProduction):Unit =
    addProduction(this, element)
}

object Nonterminal {

  object TopLevelStatements extends Nonterminal
  object BlockStatements    extends Nonterminal
  object Package            extends Nonterminal

  object Import extends Nonterminal {
    object Expression       extends Nonterminal
    object Single           extends Nonterminal
    object All              extends Nonterminal
    object Selectors        extends Nonterminal
    object Selector         extends Nonterminal
    object SimpleSelector   extends Nonterminal
    object RenamingSelector extends Nonterminal
  }

  object Template extends Nonterminal {
    object Nature      extends Nonterminal
    object Constructor extends Nonterminal
    object Body        extends Nonterminal
    object Extends     extends Nonterminal
    object Self        extends Nonterminal
    object Parents     extends Nonterminal
    object Parent      extends Nonterminal
    object InitialBody extends Nonterminal
    object New         extends Nonterminal
  }

  object Member extends Nonterminal {
    object Default extends Nonterminal
    object Val     extends Nonterminal
    object Var     extends Nonterminal
    object Ids     extends Nonterminal
    object Def     extends Nonterminal
    object Type    extends Nonterminal {
      val Parameters = Nonterminal.Type.Parameters
      val Constraint = Nonterminal.Type.Constraint
      val Assignment = Nonterminal.Type.Assignment
    }
  }

  object Pattern extends Nonterminal {
    object Bound               extends Nonterminal
    object Typed               extends Nonterminal
    object Infix               extends Nonterminal
    object Simple              extends Nonterminal
    object Underscore          extends Nonterminal
    object StringInterpolation extends Nonterminal
    object Interpolation       extends Nonterminal
    object Reference           extends Nonterminal
    object Product             extends Nonterminal
    object Sequence            extends Nonterminal
  }

  object Value {
    object Assignment           extends Nonterminal
    object Reference            extends Nonterminal
    object UnqualifiedReference extends Nonterminal
    object Parameters           extends Nonterminal
    object Parameter            extends Nonterminal
    object Application          extends Nonterminal
    object QualifiedId          extends Nonterminal
  }

  object Type {
    object Parameters          extends Nonterminal
    object Reference           extends Nonterminal
    object Constraint          extends Nonterminal
    object Single              extends Nonterminal
    object Projection          extends Nonterminal
    object VariableArity       extends Nonterminal
    object Structural          extends Nonterminal
    object Assignment          extends Nonterminal
    object ExpandVariableArity extends Nonterminal
    object Parameter           extends Nonterminal
    object Variance            extends Nonterminal
    object Application         extends Nonterminal
    object Infix               extends Nonterminal
  }

  object Expression extends Nonterminal {
    object KeywordTriggered    extends Nonterminal
    object If                  extends Nonterminal
    object Do                  extends Nonterminal
    object While               extends Nonterminal
    object Try                 extends Nonterminal
    object Catch               extends Nonterminal
    object Finally             extends Nonterminal
    object Throw               extends Nonterminal
    object For                 extends Nonterminal
    object Return              extends Nonterminal
    object Product             extends Nonterminal
    object Block               extends Nonterminal
    object Cases               extends Nonterminal
    object Case                extends Nonterminal
    object Generator           extends Nonterminal
    object Guard               extends Nonterminal
    object Application         extends Nonterminal
    object Lambda              extends Nonterminal
    object Group               extends Nonterminal
    object Interpolation       extends Nonterminal
    object InterpolationEscape extends Nonterminal
    object StringInterpolation extends Nonterminal
    object Embedded            extends Nonterminal
    object Primitive           extends Nonterminal
    object Compound            extends Nonterminal
    object Prefix              extends Nonterminal
    object Suffix              extends Nonterminal
    object Postfix             extends Nonterminal
    object Infix               extends Nonterminal
    object Simple              extends Nonterminal
    object Match               extends Nonterminal
    object Literal             extends Nonterminal
    object Number              extends Nonterminal
    object Boolean             extends Nonterminal
    object String              extends Nonterminal
  }

  object Metadata {
    object Decorations     extends Nonterminal
    object Annotation      extends Nonterminal
    object Modifier        extends Nonterminal
    object AccessModifier  extends Nonterminal
    object AccessQualifier extends Nonterminal
    object SpecialModifier extends Nonterminal
  }
}
