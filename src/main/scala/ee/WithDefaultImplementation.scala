package ee

import org.qirx.programbuilder._

trait WithDefaultImplementation[R <: Coproduct] {

  def defaultImplementationOf[T](r:ReturnWithDefault[T]):ResultProgram[T] = ReturnRunner(r)

  protected implicit val programType = ProgramType[ReturnWithDefault :+: Return :+: R]

  type ResultProgram[T] = Program[(Return :+: R)#Instance]#Instance[T]

  trait Return[T]

  abstract class ReturnWithDefault[T](default: => Program[programType.Out]#Instance[T]) {
    lazy val defaultImplementation = default
  }

  private[this] object ResultRunner extends ((Return :+: R)#Instance ~> ResultProgram) {
    def transform[x] = Program.lift
  }

  private[this] object ReturnRunner extends (ReturnWithDefault ~> ResultProgram) {
    def transform[x] = _.defaultImplementation runWith ProgramRunner
  }

  private[this] object ProgramRunner extends (programType.Out ~> ResultProgram) {
    def transform[x] = _.fold(ifHead = ReturnRunner.apply, ifTail = ResultRunner.apply)
  }
}
