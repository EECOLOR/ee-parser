package ee.ast
package types

case class TypeApplication(
  arguments : Seq[TypeReference]
)
