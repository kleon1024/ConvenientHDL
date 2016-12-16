module test 
// #(
// parameter WIDTH = 32,
// parameter LENGTH = 256
// )
(
in,
en,
out
);

input [WIDTH-1:0]   in;
wire  [WIDTH-1:0]   in;
input               en;
wire                en;
output [WIDTH-1:0]  out;
reg    [WIDTH-1:0]  out;

always @(posedge clk or posedge rst) begin
    if (rst) begin
        out <= 32'b0;
    end
    else if (en) begin
        out <= in;
    end
    else begin
        out <= out;
    end
end

endmodule