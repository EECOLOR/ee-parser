package ee

object Match extends Description(
  `match` ~ Cases
)

object Cases extends Description(
  `{` ~ Case.+ ~ `}`
)

object Case extends Description(
  `case` ~ Patterns ~ Guard.? ~ `=>` ~ BlockStatements ~ !`case`
)

object Guard extends Description(
  `if` ~ Expression
)
