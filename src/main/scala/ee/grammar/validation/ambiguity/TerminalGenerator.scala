package ee.grammar
package validation.ambiguity

trait TerminalGenerator {
  def generateFor(terminal: Terminal): String
}
