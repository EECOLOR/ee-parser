package ee

object Assignment extends Description(
  `=` ~ (`macro` ~ Reference | Expression)
)