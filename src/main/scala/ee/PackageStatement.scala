package ee

object PackageStatement extends Description(
  Annotation.* ~ `package` ~ QualifiedId ~ !`{`
)