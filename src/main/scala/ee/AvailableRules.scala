package ee

object Global {

  object TopLevelStatements extends Rule
  object BlockStatements extends Rule

  object Package extends Rule

  object Import extends Rule {
    object Expression extends Rule
    object Single extends Rule
    object All extends Rule
    object Selectors extends Rule
    object Selector extends Rule
    object SimpleSelector extends Rule
    object RenamingSelector extends Rule
  }

  object Template extends Rule {
    object Nature extends Rule
    object Constructor extends Rule
    object Body extends Rule
    object Extends extends Rule
    object Self extends Rule
    object Parents extends Rule
    object Parent extends Rule
    object InitialBody extends Rule
    object New extends Rule
  }

  object Member extends Rule {
    object Val extends Rule
    object Var extends Rule
    object Ids extends Rule
    object Def extends Rule
    object Type extends Rule {
      def Parameters = Global.Type.Parameters
      def Constraint = Global.Type.Constraint
      def Assignment = Global.Type.Assignment
    }
    object Default extends Rule
  }

  object Pattern extends Rule {
    object Bound extends Rule
    object Typed extends Rule
    object Infix extends Rule
    object Simple extends Rule
    object Underscore extends Rule
    object StringInterpolation extends Rule
    object Interpolation extends Rule
    object Reference extends Rule
    object Product extends Rule
    object Sequence extends Rule
  }

  object Value {
    object Assignment extends Rule
    object Reference extends Rule
    object UnqualifiedReference extends Rule
    object Parameters extends Rule
    object Parameter extends Rule
    object Application extends Rule
    object QualifiedId extends Rule
  }

  object Type {
    object Parameters extends Rule
    object Reference extends Rule
    object Constraint extends Rule
    object Single extends Rule
    object Projection extends Rule
    object VariableArity extends Rule
    object Structural extends Rule
    object Assignment extends Rule
    object ExpandVariableArity extends Rule
    object Parameter extends Rule
    object Variance extends Rule
    object Application extends Rule
    object Infix extends Rule
  }

  object Expression extends Rule {
    object KeywordTriggered extends Rule
    object If extends Rule
    object Do extends Rule
    object While extends Rule
    object Try extends Rule
    object Catch extends Rule
    object Finally extends Rule
    object Throw extends Rule
    object For extends Rule
    object Return extends Rule
    object Product extends Rule
    object Block extends Rule
    object Cases extends Rule
    object Case extends Rule
    object Generator extends Rule
    object Guard extends Rule
    object Application extends Rule
    object Lambda extends Rule
    object Group extends Rule
    object Interpolation extends Rule
    object InterpolationEscape extends Rule
    object StringInterpolation extends Rule
    object TickEmbedded extends Rule
    object Primitive extends Rule
    object Compound extends Rule
    object Prefix extends Rule
    object Suffix extends Rule
    object Postfix extends Rule
    object Infix extends Rule
    object Simple extends Rule
    object Match extends Rule
  }

  object Metadata {
    object Decorations extends Rule
    object Annotation extends Rule
    object Modifier extends Rule
    object AccessModifier extends Rule
    object AccessQualifier extends Rule
    object SpecialModifier extends Rule
  }
}