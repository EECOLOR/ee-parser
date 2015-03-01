package ee

object AccessModifier extends Description(
  (`public` | `private` | `protected`) ~ `[` ~ Reference ~ (`,` ~ Reference).* ~ `]`
)
