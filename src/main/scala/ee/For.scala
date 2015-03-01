package ee

object For extends Description(
  `for` ~ `{` ~ 
    Generator ~ (Generator | Member | Guard).* ~ 
  `}` ~ `yield`.? ~ Expression
)

object Generator extends Description(
  Id ~ `<-` ~ Expression ~ Guard.?
)
