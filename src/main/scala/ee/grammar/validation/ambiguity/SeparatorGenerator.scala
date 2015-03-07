package ee.grammar.validation.ambiguity

trait SeparatorGenerator {
  def generateFor(context: Path): String
}
