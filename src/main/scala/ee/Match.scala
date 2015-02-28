package ee

object Match extends Description(
  `match` ~ Cases
)

object Cases extends Description(
  `{` ~ Case.+ ~ `}`
)

object Case extends Description(
  `case` ~ CasePattern ~ Guard.? ~ `=>` ~ BlockStatements ~ !`case`
)

object CasePattern extends Description(
  Pattern ~ (`|` ~ Pattern).*
)

object Guard extends Description(
  `if` ~ Expression
)
