package ee

object Try extends Description(
  `try` ~ Expression ~ (Catch ~ Finally.? | Finally)
)

object Catch extends Description(
  `catch` ~ Expression
)

object Finally extends Description(
  `finally` ~ Expression
)
