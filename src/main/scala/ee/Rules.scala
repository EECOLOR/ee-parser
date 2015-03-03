package ee

abstract class Rules[T](scope:T) extends Scope(scope) {

  private var _rules = Set.empty[(Rule, Definition)]
  def rules = _rules

  implicit val addRule:Rules.AddRule = 
    new Rules.AddRule {
      def apply(rule: Rule, definition: Definition): Unit =
        _rules += rule -> definition
    }
}

object Rules {
  abstract class AddRule extends ((Rule, Definition) => Unit)
}