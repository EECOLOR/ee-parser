package ee.ast

import ee.ast.types.`TypeReference | ExpandVariableArity`

sealed trait `MacroReference | Expression`

// Union types would allow us to move this to another file
case class MacroReference(reference: Reference) extends `MacroReference | Expression`

sealed trait `String | Expression`

sealed trait Expression extends `MacroReference | Expression` with `String | Expression`

case class Reference(
  path: Seq[UnqualifiedReference]
) extends Expression

object Expression {
  case object Unit extends Expression

  case class If(
    predicate : Expression,
    ifTrue    : Expression,
    ifFalse   : Expression
  ) extends Expression

  case class Do(
    body      : Expression,
    predicate : Expression
  ) extends Expression

  case class While(
    predicate : Expression,
    body      : Expression
  ) extends Expression

  case class Try(
    body    : Expression,
    recover : Expression
  ) extends Expression

  // try x catch y finally z   SideEffect(Try(x, y), z)
  // try x finally y           SideEffect(x, y)
  case class SideEffect(
    body    : Expression,
    allways : Expression
  ) extends Expression

  case class Throw(
    expression : Expression
  ) extends Expression

  case class Return(
    expression : Expression
  ) extends Expression

  case class For(
    sideEffect : Boolean,
    head       : For.Generator,
    tail       : Seq[For.Element],
    body       : Expression
  ) extends Expression

  object For {
    sealed trait Element
    case class Generator(id: Id, body: Expression, guard: Expression) extends Element
    case class Guard(guard: Expression) extends Element
    // we could really use union types and set tail to `For.Element | Member`
    case class WrappedMember(member: Member) extends Element
  }

  case class Embedded(
    id   : Id,
    body : String
  ) extends Expression

  case class Lambda(
    parameters : Seq[ParameterList],
    body       : Expression
  ) extends Expression

  case class PrefixApplication(
    operator : UnqualifiedReference,
    target   : Expression
  ) extends Expression

  case class PostfixApplication(
    target   : Expression,
    operator : UnqualifiedReference
  ) extends Expression

  case class Application(
    target    : Expression,
    operator  : UnqualifiedReference,
    arguments : Seq[ArgumentList]
  ) extends Expression

  case class TypeAscription(
    target   : Expression,
    ascribed : `TypeReference | ExpandVariableArity`
  ) extends Expression

  case class Match(
    target : Expression,
    cases  : Cases
  ) extends Expression

  case class Case(
    exposedIds : Seq[Id],
    predicate  : Expression,
    body       : Expression
  ) extends Expression

  case class Product(
    expressions : Expression
  ) extends Expression

  case class Cases(
    cases: Case *
  ) extends Expression

  case class Block(
    expressions : Expression *
  ) extends Expression

  case class StringInterpolation(
    handler : Reference,
    body    : Seq[`String | Expression`]
  )

  case class NumericValue[T : Numeric](
    value: T
  ) extends Expression

  sealed trait BooleanValue extends Expression
  case object True  extends BooleanValue
  case object False extends BooleanValue

  case class StringValue(
    value: String
  ) extends Expression with `String | Expression`

  case class Symbol(
    value: String
  ) extends Expression

  case object NullValue extends Expression

  case class ConvertToLambda(
    expression : Expression
  ) extends Expression

  case class Instantiate(
    initialBody    : Template.Body,
    parents        : Seq[Template.Parent],
    body           : Template.Body
  )
}
