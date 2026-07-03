// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tile._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

class TraceSinkArbiter(nSeq : Seq[Int], use_monitor: Boolean = false, monitor_name: String = "unknown") extends Module {
  val io = IO(new Bundle {
    val target = Input(UInt(TraceSinkTarget.width.W))
    val in = Flipped(new TraceEgressInterface())
    val out = Vec(nSeq.size, new TraceEgressInterface())
  })
  val nVec = VecInit(nSeq.map(_.U))
  io.in.ready := Mux(nVec.contains(io.target), io.out(nVec.indexWhere(_ === io.target)).ready, true.B)
  io.out.zipWithIndex.foreach { case (o, i) => 
    o.valid := io.in.valid && (io.target === nVec(i))
    o.bits := io.in.bits
    o.mask := io.in.mask
  }

  if (use_monitor) {
    val monitor = Module(new TraceSinkMonitor(s"trace_monitor_$monitor_name.encoded.trace"))
    monitor.io.in_fire := io.in.valid && io.in.ready
    monitor.io.in_byte := io.in.bits
    monitor.io.in_mask := io.in.mask
    monitor.io.clk := clock
    monitor.io.reset := reset
  }
}
