// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import scala.math.min

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

case class TraceEncoderParams(
  encoderBaseAddr: BigInt,
  buildEncoder: Parameters => LazyTraceEncoder,
  useArbiterMonitor: Boolean,
  // a seq of functions that takes a parameter and returns a lazymodule and a target id
  buildSinks: Seq[Parameters => (LazyTraceSink, Int)] = Seq.empty[Parameters => (LazyTraceSink, Int)]
)

class LazyTraceEncoder(val coreParams: TraceCoreParams)(implicit p: Parameters) extends LazyModule {
  override lazy val module = new LazyTraceEncoderModule(this)
  override def shouldBeInlined = false
}

class LazyTraceEncoderModule(outer: LazyTraceEncoder) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val control = Input(new TraceEncoderControlInterface())
    val in = Input(new TraceCoreInterface(outer.coreParams))
    val stall = Output(Bool())
    val out = new TraceEgressInterface()
  })
}