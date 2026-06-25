// See LICENSE.SiFive for license details.
//VCS coverage exclude_file
`ifndef RESET_DELAY
 `define RESET_DELAY 777.7
`endif
`ifndef MODEL
 `define MODEL TestHarness
`endif

module TestDriver;

`ifdef DEBUG
  // v 5.022 silently drops $dumpoff/$dumpon when emitting C++.
  // wf_set_dumping is provided by sims/verilator/wf_main.cc and flips the
  // model's internal __Vm_dumping flag, which is the only effective way to
  // pause/resume the trace stream at runtime.
  import "DPI-C" function void wf_set_dumping(input bit en);
`endif

  reg clock = 1'b0;
  reg reset = 1'b1;

  always #(`CLOCK_PERIOD/2.0) clock = ~clock;
  initial #(`RESET_DELAY) reset = 0;

  // Read input arguments and initialize
  reg verbose = 1'b0;
  wire printf_cond = verbose && !reset;
  reg [63:0] max_cycles = 0;
  reg [63:0] dump_start = 0;
  reg [63:0] trace_count = 0;
  reg [2047:0] fsdbfile = 0;
  reg [2047:0] vcdplusfile = 0;
  reg [2047:0] vcdfile = 0;
  int unsigned rand_value;
  initial
  begin
    void'($value$plusargs("max-cycles=%d", max_cycles));
    void'($value$plusargs("dump-start=%d", dump_start));
    verbose = $test$plusargs("verbose");

    // do not delete the lines below.
    // $random function needs to be called with the seed once to affect all
    // the downstream $random functions within the Chisel-generated Verilog
    // code.
    // $urandom is seeded via cmdline (+ntb_random_seed in VCS) but that
    // doesn't seed $random.
    rand_value = $urandom;
    rand_value = $random(rand_value);
    if (verbose) begin
`ifdef VCS
      $fdisplay(stderr, "testing $random %0x seed %d", rand_value, unsigned'($get_initial_random_seed));
`else
      $fdisplay(stderr, "testing $random %0x", rand_value);
`endif
    end

`ifdef DEBUG

    if ($value$plusargs("vcdplusfile=%s", vcdplusfile))
    begin
`ifdef VCS
      $vcdplusfile(vcdplusfile);
`else
      $fdisplay(stderr, "Error: +vcdplusfile is VCS-only; use +vcdfile instead or recompile with VCS=1");
      $fatal;
`endif
    end

    if ($value$plusargs("fsdbfile=%s", fsdbfile))
    begin
`ifdef FSDB
      $fsdbDumpfile(fsdbfile);
      $fsdbDumpvars("+all");
      //$fsdbDumpSVA;
`else
      $fdisplay(stderr, "Error: +fsdbfile is FSDB-only; use +vcdfile/+vcdplus instead or recompile with FSDB=1");
      $fatal;
`endif
    end

    if ($value$plusargs("vcdfile=%s", vcdfile))
    begin
      $dumpfile(vcdfile);
      $dumpvars(0, testHarness);
`ifdef DEBUG
      // Start tracing disabled. The wf_active edge-detect block below
      // toggles it on/off based on the harness selective-waveform binder.
      // Builds without the binder default wf_active=1, so the very first
      // clock edge sees a 0->1 transition and turns dumping on.
      wf_set_dumping(1'b0);
`endif
    end

`ifdef FSDB
`define VCDPLUSON $fsdbDumpon;
`define VCDPLUSOFF $fsdbDumpoff;
`define VCDPLUSCLOSE $fsdbDumpoff;
`elsif VCS
`define VCDPLUSON $vcdpluson(0); $vcdplusmemon(0);
`define VCDPLUSOFF $vcdplusoff;
`define VCDPLUSCLOSE $vcdplusclose; $dumpoff;
`else
`define VCDPLUSON $dumpon;
`define VCDPLUSOFF $dumpoff;
`define VCDPLUSCLOSE $dumpoff;
`endif
`else
  // No +define+DEBUG
`define VCDPLUSON
`define VCDPLUSCLOSE

    if ($test$plusargs("vcdplusfile=") || $test$plusargs("vcdfile=") || $test$plusargs("fsdbfile="))
    begin
      $fdisplay(stderr, "Error: +vcdfile, +vcdplusfile, or +fsdbfile requested but compile did not have +define+DEBUG enabled");
      $fatal;
    end

`endif

    // Note: the previous initial-block VCDPLUSON (gated on dump_start == 0)
    // was removed because the wf_active edge-detect block below is the sole
    // arbiter of dump on/off. For backward-compat builds, wf_active defaults
    // to 1, so the very first posedge clock fires a 0->1 rising edge and
    // turns dumping back on. The trade-off: pre-clock initial-state values
    // are not captured (acceptable; reset sequence still appears once the
    // first clock fires).
  end

`ifdef TESTBENCH_IN_UVM
  // UVM library has its own way to manage end-of-simulation.
  // A UVM-based testbench will raise an objection, watch this signal until this goes 1, then drop the objection.
  reg finish_request = 1'b0;
`endif
  reg [255:0] reason = "";
  reg failure = 1'b0;
  wire success;
  integer stderr = 32'h80000002;
  integer stdout = 32'h80000001;
  always @(posedge clock)
  begin
`ifdef GATE_LEVEL
    if (verbose)
    begin
      $fdisplay(stderr, "C: %10d", trace_count);
    end
`endif

    trace_count = trace_count + 1;

    if (trace_count == dump_start)
    begin
      `VCDPLUSON
    end

    if (!reset)
    begin
      if (max_cycles > 0 && trace_count > max_cycles)
      begin
        reason = " (timeout)";
        failure = 1'b1;
      end

      if (failure)
      begin
        $fdisplay(stderr, "*** FAILED ***%s after %d simulation cycles", reason, trace_count);
        `VCDPLUSCLOSE
        $fatal;
      end

      if (success)
      begin
        if (verbose)
          $fdisplay(stderr, "*** PASSED *** Completed after %d simulation cycles", trace_count);
        `VCDPLUSCLOSE
`ifdef TESTBENCH_IN_UVM
        finish_request = 1;
`else
        $finish;
`endif
      end
    end
  end

  // Selective waveform dumping driven by harness wf_active.
  // Tracing was set to disabled at the end of the initial block via $dumpoff;
  // wf_active_d defaults to 0, so the first posedge clock sees a 0->1 edge
  // when wf_active is high (backward-compat builds where wf_active=1 by
  // default), turning dumping back on. For binder builds, wf_active stays 0
  // until a window triggers, then the rising edge fires.
  wire wf_active;
`ifdef DEBUG
  reg wf_active_d;
  always @(posedge clock) begin
    wf_active_d <= wf_active;
    if (wf_active && !wf_active_d) begin
      $fdisplay(stdout, "WF: rising edge at t=%0t cycle=%0d", $time, trace_count);
      wf_set_dumping(1'b1);
    end
    if (!wf_active && wf_active_d) begin
      $fdisplay(stdout, "WF: falling edge at t=%0t cycle=%0d", $time, trace_count);
      wf_set_dumping(1'b0);
    end
  end
`endif

  `MODEL testHarness(
    .clock(clock),
    .reset(reset),
    .io_success(success),
    .io_wf_active(wf_active)
  );

endmodule
