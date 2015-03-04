package ee

import org.qirx.programbuilder._

trait WithDefaultImplementation[Result <: Coproduct] {

  def defaultImplementationOf[T](r:ReturnWithDefault[T]):ResultProgram[T] = ReturnRunner(r)

  protected implicit val programType = ProgramType[ReturnWithDefault :+: Return :+: Result]

  type ResultProgram[T] = Program[(Return :+: Result)#Instance]#Instance[T]

  trait Return[T]

  abstract class ReturnWithDefault[T](default: => Program[programType.Out]#Instance[T]) {
    lazy val defaultImplementation = default
  }

  private object ResultRunner extends ((Return :+: Result)#Instance ~> ResultProgram) {
    def transform[x] = Program.lift
  }

  private object ReturnRunner extends (ReturnWithDefault ~> ResultProgram) {
    def transform[x] = _.defaultImplementation runWith ProgramRunner
  }

  private object ProgramRunner extends (programType.Out ~> ResultProgram) {
    def transform[x] = _.fold(ifHead = ReturnRunner.apply, ifTail = ResultRunner.apply)
  }
}