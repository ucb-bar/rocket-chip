module TraceSinkMonitor
#(
    parameter FILE_NAME = "trace_sink_monitor.txt"
)
(
    input clk,
    input reset,
    input in_fire,
    input[7:0] in_byte_0,
    input[7:0] in_byte_1,
    input[7:0] in_byte_2,
    input[7:0] in_byte_3,
    input in_mask_0,
    input in_mask_1,
    input in_mask_2,
    input in_mask_3
);

`ifndef SYNTHESIS

integer file;

initial begin
    file = $fopen(FILE_NAME, "w");
    if (file == 0) begin
        $display("Failed to open %s", FILE_NAME);
        $finish;
    end
end

always @(posedge clk) begin
    if (in_fire & ~reset) begin
        if (in_mask_0) begin
            $fwrite(file, "%c", in_byte_0);
        end
        if (in_mask_1) begin
            $fwrite(file, "%c", in_byte_1);
        end
        if (in_mask_2) begin
            $fwrite(file, "%c", in_byte_2);
        end
        if (in_mask_3) begin
            $fwrite(file, "%c", in_byte_3);
        end
    end
end 

final begin
    $fclose(file);
end

`endif

endmodule