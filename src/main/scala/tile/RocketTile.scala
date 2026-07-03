// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import chisel3._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{BasicBusBlockerParams, BasicBusBlocker}
import org.chipsalliance.diplomacy.DisableMonitors
import freechips.rocketchip.diplomacy.{
  AddressSet, BufferParams
}
import freechips.rocketchip.resources.{
  SimpleDevice, Description,
  ResourceAnchors, ResourceBindings, ResourceBinding, Resource, ResourceAddress,
}
import freechips.rocketchip.interrupts.IntIdentityNode
import freechips.rocketchip.tilelink.{TLIdentityNode, TLBuffer}
import freechips.rocketchip.rocket.{
  RocketCoreParams, ICacheParams, DCacheParams, BTBParams, HasHellaCache,
  HasICacheFrontend, ScratchpadSlavePort, HasICacheFrontendModule, Rocket
}
import freechips.rocketchip.subsystem.HierarchicalElementCrossingParamsLike
import freechips.rocketchip.prci.{ClockSinkParameters, RationalCrossing, ClockCrossingType}
import freechips.rocketchip.util.InOrderArbiter
import freechips.rocketchip.trace.{TraceEncoderParams,TraceEncoderController, TraceSinkArbiter}
import freechips.rocketchip.subsystem._

import freechips.rocketchip.util.BooleanToAugmentedBoolean

case class RocketTileBoundaryBufferParams(force: Boolean = false)

case class RocketTileParams(
    core: RocketCoreParams = RocketCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    tileId: Int = 0,
    beuAddr: Option[BigInt] = None,
    blockerCtrlAddr: Option[BigInt] = None,
    clockSinkParams: ClockSinkParameters = ClockSinkParameters(),
    boundaryBuffers: Option[RocketTileBoundaryBufferParams] = None,
    traceParams: Option[TraceEncoderParams] = None
  ) extends InstantiableTileParams[RocketTile] {
  require(icache.isDefined)
  require(dcache.isDefined)
  val baseName = "rockettile"
  val uniqueName = s"${baseName}_$tileId"
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): RocketTile = {
    new RocketTile(this, crossing, lookup)
  }
}

class RocketTile private(
      val rocketParams: RocketTileParams,
      crossing: ClockCrossingType,
      lookup: LookupByHartIdImpl,
      q: Parameters)
    extends BaseTile(rocketParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with HasHellaCache
    with HasICacheFrontend
{
  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: RocketTileParams, crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = rocketParams.beuAddr map { _ => IntIdentityNode() }
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map { s =>
    LazyModule(new ScratchpadSlavePort(AddressSet.misaligned(s, d.dataScratchpadBytes), lazyCoreParamsView.coreDataBytes, tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO))
  }}
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, lm.node.portParams.head.beatBytes))

  val bus_error_unit = rocketParams.beuAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a), xLen/8))
    intOutwardNode.get := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  /**
   * A Centralized Trace encoder controller,
   * controlling enable/disable of trace encoder
   * and selecting trace sink
   */
  val trace_encoder_controller = rocketParams.traceParams.map { t =>
    val trace_encoder_controller = LazyModule(new TraceEncoderController(t.encoderBaseAddr, xBytes, tileId))
    connectTLSlave(trace_encoder_controller.node, xBytes)
    trace_encoder_controller
  }

  val trace_encoder = rocketParams.traceParams match {
    case Some(t) => Some(t.buildEncoder(p))
    case None => None
  }

  val (trace_sinks, traceSinkIds) = rocketParams.traceParams match {
    case Some(t) => t.buildSinks.map {_(p)}.unzip
    case None => (Nil, Nil)
  }

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt + rocketParams.core.vector.map(_.useDCache.toInt).getOrElse(0)

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = frontend.icache.itimProperty.toSeq.flatMap(p => Map("sifive,itim" -> p))

  val beuProperty = bus_error_unit.map(d => Map(
          "sifive,buserror" -> d.device.asProperty)).getOrElse(Nil)

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("sifive,rocket0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty
                  ++ tileProperties ++ dtimProperty ++ itimProperty ++ beuProperty)
    }
  }

  val vector_unit = rocketParams.core.vector.map(v => LazyModule(v.build(p)))
  vector_unit.foreach(vu => tlMasterXbar.node :=* vu.atlNode)
  vector_unit.foreach(vu => tlOtherMastersNode :=* vu.tlNode)


  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
  }

  override lazy val module = new RocketTileModuleImp(this)

  override def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = (rocketParams.boundaryBuffers, crossing) match {
    case (Some(RocketTileBoundaryBufferParams(true )), _)                   => TLBuffer()
    case (Some(RocketTileBoundaryBufferParams(false)), _: RationalCrossing) => TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
    case _ => TLBuffer(BufferParams.none)
  }

  override def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = (rocketParams.boundaryBuffers, crossing) match {
    case (Some(RocketTileBoundaryBufferParams(true )), _)                   => TLBuffer()
    case (Some(RocketTileBoundaryBufferParams(false)), _: RationalCrossing) => TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
    case _ => TLBuffer(BufferParams.none)
  }
}

class RocketTileModuleImp(outer: RocketTile) extends BaseTileModuleImp(outer)
    with HasFpuOpt
    with HasLazyRoCCModule
    with HasICacheFrontendModule {
  val core = Module(new Rocket(outer)(outer.p))
  outer.vector_unit.foreach { v =>
    core.io.vector.get <> v.module.io.core
    v.module.io.tlb <> outer.dcache.module.io.tlb_port
  }

  // reset vector is connected in the Frontend to s2_pc
  core.io.reset_vector := DontCare

  if (outer.rocketParams.traceParams.isDefined) {
    core.io.trace_core_ingress.get <> outer.trace_encoder.get.module.io.in
    outer.trace_encoder_controller.foreach { lm =>
      outer.trace_encoder.get.module.io.control <> lm.module.io.control
      lm.module.io.perf.stall := outer.trace_encoder.get.module.io.stall
    }

    val trace_sink_arbiter = Module(new TraceSinkArbiter(outer.traceSinkIds, 
      use_monitor = outer.rocketParams.traceParams.get.useArbiterMonitor, 
      monitor_name = outer.rocketParams.uniqueName))

    trace_sink_arbiter.io.target := outer.trace_encoder.get.module.io.control.target
    trace_sink_arbiter.io.in <> outer.trace_encoder.get.module.io.out 


    core.io.traceStall := outer.traceAuxSinkNode.bundle.stall || outer.trace_encoder.get.module.io.stall

    outer.trace_sinks.zip(outer.traceSinkIds).foreach { case (sink, id) =>
      val index = outer.traceSinkIds.indexOf(id)
      sink.module.io.trace_in <> trace_sink_arbiter.io.out(index)
    }
  } else {
    core.io.traceStall := outer.traceAuxSinkNode.bundle.stall
  }

  // Report unrecoverable error conditions; for now the only cause is cache ECC errors
  outer.reportHalt(List(outer.dcache.module.io.errors))

  // Report when the tile has ceased to retire instructions; for now the only cause is clock gating
  outer.reportCease(outer.rocketParams.core.clockGate.option(
    !outer.dcache.module.io.cpu.clock_enabled &&
    !outer.frontend.module.io.cpu.clock_enabled &&
    !ptw.io.dpath.clock_enabled &&
    core.io.cease))

  outer.reportWFI(Some(core.io.wfi))

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  outer.bus_error_unit.foreach { beu =>
    core.io.interrupts.buserror.get := beu.module.io.interrupt
    beu.module.io.errors.dcache := outer.dcache.module.io.errors
    beu.module.io.errors.icache := outer.frontend.module.io.errors
  }

  core.io.interrupts.nmi.foreach { nmi => nmi := outer.nmiSinkNode.get.bundle }

  // Pass through various external constants and reports that were bundle-bridged into the tile
  outer.traceSourceNode.bundle <> core.io.trace
  
  outer.bpwatchSourceNode.bundle <> core.io.bpwatch
  core.io.hartid := outer.hartIdSinkNode.bundle
  require(core.io.hartid.getWidth >= outer.hartIdSinkNode.bundle.getWidth,
    s"core hartid wire (${core.io.hartid.getWidth}b) truncates external hartid wire (${outer.hartIdSinkNode.bundle.getWidth}b)")

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.imem
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu =>
    core.io.fpu :<>= fpu.io.waiveAs[FPUCoreIO](_.cp_req, _.cp_resp)
  }
  if (fpuOpt.isEmpty) {
    core.io.fpu := DontCare
  }
  outer.vector_unit foreach { v => if (outer.rocketParams.core.vector.get.useDCache) {
    dcachePorts += v.module.io.dmem
  } else {
    v.module.io.dmem := DontCare
  } }
  core.io.ptw <> ptw.io.dpath

  // Connect the coprocessor interfaces
  if (outer.roccs.size > 0) {
    cmdRouter.get.io.in <> core.io.rocc.cmd
    outer.roccs.foreach{ lm =>
      lm.module.io.exception := core.io.rocc.exception
      lm.module.io.fpu_req.ready := DontCare
      lm.module.io.fpu_resp.valid := DontCare
      lm.module.io.fpu_resp.bits.data := DontCare
      lm.module.io.fpu_resp.bits.exc := DontCare
    }
    core.io.rocc.resp <> respArb.get.io.out
    core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_ || _))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_ || _)
    (core.io.rocc.csrs zip roccCSRIOs.flatten).foreach { t => t._2 <> t._1 }
  } else {
    // tie off
    core.io.rocc.cmd.ready := false.B
    core.io.rocc.resp.valid := false.B
    core.io.rocc.resp.bits := DontCare
    core.io.rocc.busy := DontCare
    core.io.rocc.interrupt := DontCare
  }
  // Dont care mem since not all RoCC need accessing memory
  core.io.rocc.mem := DontCare

  // Rocket has higher priority to DTIM than other TileLink clients
  outer.dtim_adapter.foreach { lm => dcachePorts += lm.module.io.dmem }

  // TODO eliminate this redundancy
  val h = dcachePorts.size
  val c = core.dcacheArbPorts
  val o = outer.nDCachePorts
  require(h == c, s"port list size was $h, core expected $c")
  require(h == o, s"port list size was $h, outer counted $o")
  // TODO figure out how to move the below into their respective mix-ins
  dcacheArb.io.requestor <> dcachePorts.toSeq
  ptw.io.requestor <> ptwPorts.toSeq
}

trait HasFpuOpt { this: RocketTileModuleImp =>
  val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
  fpuOpt.foreach { fpu =>
    val nRoCCFPUPorts = outer.roccs.count(_.usesFPU)
    val nFPUPorts = nRoCCFPUPorts + outer.rocketParams.core.useVector.toInt
    if (nFPUPorts > 0) {
      val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
      fpu.io.cp_req <> fpArb.io.out_req
      fpArb.io.out_resp <> fpu.io.cp_resp

      val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
      for (i <- 0 until nRoCCFPUPorts) {
        fpArb.io.in_req(i) <> fp_rocc_ios(i).fpu_req
        fp_rocc_ios(i).fpu_resp <> fpArb.io.in_resp(i)
      }
      outer.vector_unit.foreach(vu => {
        fpArb.io.in_req(nRoCCFPUPorts) <> vu.module.io.fp_req
        vu.module.io.fp_resp <> fpArb.io.in_resp(nRoCCFPUPorts)
      })
    } else {
      fpu.io.cp_req.valid := false.B
      fpu.io.cp_req.bits := DontCare
      fpu.io.cp_resp.ready := false.B
    }
  }
}
