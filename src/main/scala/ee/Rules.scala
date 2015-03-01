package ee

abstract class Rules[T](scope:T) extends Scope(scope) {

  private var _rules = Set.empty[(Rule, Definition)]
  def rules = _rules

  implicit def addRule(rule: Rule, definition: Definition): Unit =
    _rules += rule -> definition
}