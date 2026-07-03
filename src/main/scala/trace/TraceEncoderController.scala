// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.resources.{Description, ResourceBindings, SimpleDevice}
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

object TraceSinkTarget {
  def width = 8
}

class TraceEncoderControlInterface() extends Bundle {
  val enable = Bool()
  val target = UInt(TraceSinkTarget.width.W)
  val bp_mode = UInt(32.W)
}

class TraceEncoderPerformanceInterface() extends Bundle {
  val stall = Bool()
}

class TraceEncoderController(addr: BigInt, beatBytes: Int, hartId: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice(s"trace-encoder-controller$hartId", Seq("ucbbar,trace")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val traceDmaProp = Map(
        "ucbbar,trace-dma" -> resources("ucbbar,trace-dma").map(_.value)
      ).collect { case (k, v) if v.nonEmpty => (k, v) }
      Description(name, mapping ++ traceDmaProp)
    }
  }
  val node = TLRegisterNode(
    address = Seq(AddressSet(addr, 0xFF)),
    device = device,
    beatBytes = beatBytes
  )
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val control = Output(new TraceEncoderControlInterface())
      val perf = Input(new TraceEncoderPerformanceInterface())
    })

    val control_reg_write_valid = Wire(Bool())
    val control_reg_bits = RegInit(1.U(2.W))
    val enable = control_reg_bits(1)
    val active = control_reg_bits(0)
    io.control.enable := enable

    val trace_encoder_impl = RegInit(0.U(32.W))

    val trace_sink_target = RegInit(0.U(TraceSinkTarget.width.W))
    io.control.target := trace_sink_target.asUInt

    val trace_bp_mode = RegInit(0.U(32.W))
    io.control.bp_mode := trace_bp_mode

    def traceEncoderControlRegWrite(valid: Bool, bits: UInt): Bool = {
      control_reg_write_valid := valid
      when (control_reg_write_valid) {
        control_reg_bits := bits
      }
      true.B
    }

    def traceEncoderControlRegRead(ready: Bool): (Bool, UInt) = {
      (true.B, control_reg_bits)
    }

    val stall = RegInit(0.U(64.W))
    when (io.perf.stall) {
      stall := stall + 1.U
    }

    val regmap = node.regmap(
      Seq(
        0x00 -> Seq(
          RegField(2, traceEncoderControlRegRead(_), traceEncoderControlRegWrite(_, _),
            RegFieldDesc("control", "Control trace encoder"))
        ),
        0x04 -> Seq(
          RegField.r(32, trace_encoder_impl,
            RegFieldDesc("impl", "Trace encoder implementation"))
        ),
        0x20 -> Seq(
          RegField(8, trace_sink_target,
            RegFieldDesc("target", "Trace sink target"))
        ),
        0x24 -> Seq(
          RegField(32, trace_bp_mode,
            RegFieldDesc("bp_mode", "Trace branch predictor mode"))
        ),
        0x28 -> Seq(
          RegField.r(64, stall,
            RegFieldDesc("stall", "Trace encoder stall"))
        )
      ):_*
    )
  }
}
