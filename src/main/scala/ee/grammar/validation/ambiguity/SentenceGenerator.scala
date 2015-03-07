package ee.grammar
package validation.ambiguity

trait SentenceGenerator {
  def generate(nonterminal: Nonterminal): String
}
