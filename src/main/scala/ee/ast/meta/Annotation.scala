package ee.ast
package meta

case class Annotation(
  reference : Reference,
  arguments : Seq[ArgumentList]
)
