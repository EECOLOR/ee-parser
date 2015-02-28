package ee

object Pattern extends Description(
  TypedPattern | BoundPattern
)

object TypedPattern extends Description(
  (Id | `_`) ~ AssignedType
)

object BoundPattern extends Description(
  (Id ~ `@`).? ~ (ExpandVariableArity | InfixPattern)
)

object InfixPattern extends Description(
  SimplePattern ~ (Id ~ SimplePattern).*
)

object SimplePattern extends Description(
  UnderscorePattern | StringInterpolationPattern | Literals | ProductPattern | Id
)

object UnderscorePattern extends Description(
  `_` ~ AssignedType.?
)

object StringInterpolationPattern extends Description(
  QualifiedId ~ `"` ~ (InterpolationPattern ~ !`"`) ~ `"`
)

object InterpolationPattern extends Description(
  InterpolationEscape | `$` ~ Id | `$` ~ `{` ~ CasePattern ~ `}`
)

object ProductPattern extends Description(
  (QualifiedId ~ TypeParameters.*).? ~ `(` ~ CasePattern ~ (`,` ~ CasePattern).* ~`)`
)