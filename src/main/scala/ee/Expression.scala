package ee

object Expression extends Description(
  TickEmbedded |
  Lambda       |
  If           |
  While        |
  Try          |
  Do           |
  For          |
  Throw        |
  Return       |
  SimpleExpression ~ ExpressionSuffix.?
)

object SimpleExpression extends Description(
 PrefixOperator.? ~
 (
   New                 |
   Cases               |
   Block               |
   ProductExpression   |
   StringInterpolation |
   Literal             |
   TypeReference       |
   Reference           |
   `_`
 ) ~
 (
   (`.` ~ Id)      |
   Application
  ).* ~
  `_`.?
)

object ExpressionSuffix extends Description(
  Match | AssignedType | Assignment
)

object ProductExpression extends Description(
  `(` ~ Expression ~ (`,` ~ Expression).* ~ `)`
)
