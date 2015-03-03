package ee

import org.qirx.programbuilder._

trait WithDefaultImplementation[Result <: Coproduct] {

  def defaultImplementationOf[T](r:Return[T]):ResultProgram[T] = ReturnRunner(r)

  protected implicit val programType = ProgramType[Return :+: Result]

  type ResultProgram[T] = Program[Result#Instance]#Instance[T]

  abstract class Return[T](default: => Program[(Return :+: Result)#Instance]#Instance[T]) {
    lazy val defaultImplementation = default
  }

  private object ResultRunner extends (Result#Instance ~> ResultProgram) {
    def transform[x] = Program.lift
  }

  private object ReturnRunner extends (Return ~> ResultProgram) {
    def transform[x] = _.defaultImplementation runWith ProgramRunner
  }

  private object ProgramRunner extends (programType.Out ~> ResultProgram) {
    def transform[x] = _.fold(ifHead = ReturnRunner.apply, ifTail = ResultRunner.apply)
  }
}