package ee

object BlockStatements extends Description(
  (ImportStatement | Template | Member |Expression).*
)