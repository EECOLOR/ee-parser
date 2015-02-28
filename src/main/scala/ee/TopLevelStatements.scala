package ee

object TopLevelStatements extends Description(
  (PackageStatement | ImportStatement | Template | Member).*
)