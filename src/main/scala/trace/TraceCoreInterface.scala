// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._

// Definitions for Trace core Interface defined in RISC-V Processor Trace Specification V1.0
object TraceItype extends ChiselEnum {
  val ITNothing   = Value(0.U)
  val ITException = Value(1.U)
  val ITInterrupt = Value(2.U)
  val ITExcReturn = Value(3.U)
  val ITBrNTaken  = Value(4.U)
  val ITBrTaken   = Value(5.U)
  val ITReserved6 = Value(6.U)
  val ITReserved7 = Value(7.U)
  val ITUnCall    = Value(8.U)
  val ITInCall    = Value(9.U)
  val ITUnTail    = Value(10.U)
  val ITInTail    = Value(11.U)
  val ITCoSwap    = Value(12.U)
  val ITReturn    = Value(13.U)
  val ITUnJump    = Value(14.U)
  val ITInJump    = Value(15.U)
}

object TraceEgressConstants {
  val numLanes = 4
}

case class TraceCoreParams (
  nGroups: Int = 1,
  iretireWidth: Int = 1,
  xlen: Int = 64,
  iaddrWidth: Int = 64
)


class TraceCoreGroup (val params: TraceCoreParams) extends Bundle {
  val iretire = UInt(params.iretireWidth.W)
  val iaddr = UInt(params.iaddrWidth.W)
  val itype = TraceItype()
  val ilastsize = UInt(1.W)
}

class TraceCoreInterface (val params: TraceCoreParams) extends Bundle {
  val group = Vec(params.nGroups, new TraceCoreGroup(params))
  val priv = UInt(4.W)
  val ctx = UInt(params.xlen.W)
  val tval = UInt(params.xlen.W)
  val cause = UInt(params.xlen.W)
  val time = UInt(params.xlen.W)
}

class TraceEgressInterface extends Bundle {
  val bits = Output(Vec(TraceEgressConstants.numLanes, UInt(8.W)))
  val mask = Output(Vec(TraceEgressConstants.numLanes, Bool()))
  val valid = Output(Bool())
  val ready = Input(Bool())
  def fire = valid && ready
}