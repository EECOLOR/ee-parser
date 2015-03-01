package ee

object ParentTypes extends Description(
  ParentType ~ (`with` ~ ParentType).*
)

object ParentType extends Description(
  TypeReference ~ Application.*
)
