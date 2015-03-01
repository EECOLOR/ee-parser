package ee

object Try extends Description(
  `try` ~ Expression ~ (`catch` ~ Expression ~ Finally.? | Finally)
)

object Finally extends Description(
  `finally` ~ Expression
)
