<!-- Readme of Convenient Hardware Description Language -->
##Convenient Coding HDL (Verilog)

###Description

* Fast instantiation of Verilog top module with Cynide (defined description file).

* Add instances to the original Verilog with Cynide.

<!-- * Converts Convenient Coding Verilog (.ccv) into Verilog (.v) to generate top module file rapidly. Automatically get children's port descriptions from Verilog 1995, Verilog 2001, Verilog 2005(ports conversion), VHDL. For IP cores, only port description file is needed. Users may write serveral IPs' ports descriptions in one file. -->

<!-- * Change the port format of Verilog File between Verilog 1995, Verilog 2001 and Verilog 2005. Clear or add annotations or port direction descriptions. Merge or split Verilog files.

* Prints out the project hierarchy.

* Provides basic circuits check and automatically fix the potential grammar problem. -->

###Compile

```Bash
git clone https://github.com/githubkleon/ConvenientHDL.git
cd ConvenientHDL
cd src
ghc ccv.hs
```

###Argument

* --init (filepath) :: (initial) Create a project tree for the files under filepath recursively. If no filepath is specified, then use current directory by default.

* --email [emailaddress] :: (email) Set default contact e-mail address for file header. Default is null(""), which won't be shown out in file header.

* --author [authorname] :: (author) Set default user name for file header.  Default is null("")

<!-- , which won't be shown out in file header. -->

<!--     Scans the root path, if multiple top module found, user have to select the top file manually.
    If there exists a previous project file .ccv.project. This file will be updated. -->

* --update (filepath) :: (update) Update current project hierarchy. Recurrsively search parental directoy until find project file.

* -n (filename) :: (new) Create a normal new verilog file with module frame description and right claimation. 

<!-- "a.v" is used by default. If filename has exist, choice box will prompt. The Revision number is set to v0.1 automatically.  -->

* -t [topfilename] [filepath] :: (top) Automaticlly generate a top module from Cynide description.

<!-- There are two ways to generate a top file: -->

<!--     If there is a file with the same name as topfilename ends with file extension name (ccv), then a "topefilename.v" is generated according to "topfilename.ccv". -->

<!--     -If the parameter of the instantied module is the same as the topfilename module, ccv would automatically connect them, otherwise the value from child module fills it. 

    -If the clock and reset ports are the same as the topfilename module, and -a option is checked, ccv would automatically connect them. Otherwise user may have to set the clk manually, or leave ports unconnected. -->

* -d :: (desciption) Preserves port comments in child module. If not specified, no annotion is provided. (--TODO)

* -a :: (automatic) Automatically connects the ports of top module and instatitaed module with the same name as instatitaed modules. If not specified, all ports will be left unconnected.
(--TODO)

* -p :: (ports) Show the port directions of intances as comments.

* -i :: (Increment) Merge converted file into original file. Merge the rest of files into the first one.

* -v [revision] [filename] (description) :: (revision) Updates revision and adds descriptions. If specifies +0.1, the default revision will be changed to step. If there is no revision specified, a revision will be added between module desciption and header. (-TODO)

* -H (filename) :: (hierarchy) Shows the project hierarchy to a file or to screen.

* -f [filename] :: (formatize) Pretty print Verilog file.

<!-- * -f v95 v01 filename :: (formatize) Converts the port description. The two parameter followed -f can be: v95(v1995), v(v01, v2001), sv(v05, v2005), VHDL, std. Specially, a conversion between v2001 and VHDL is provided.  -->

<!-- Specially any file can be converted to std, this converts all tabs into four spaces and formats all brackets and begin-end pairs. -->

* -m (filelist) :: (merge) Merges multiple files. The filelist is a series of filename. The filelist can be infinite. Merge current directory files by default. The original files are renamed with extension .old.

* -s :: (split) Splits a single file into serveral module description files. The original file is renamed with extension .old.

###Usages

Take windows as an example.


* Add the path of ccv (execute file) into PATH.

* Enter the root of project directory (can be empty).

```
F:\Soft\Haskell\ccvtest>dir
 Volume in drive F is Project
 Volume Serial Number is 32C7-6C5E

 Directory of F:\Soft\Haskell\ccvtest

2016/12/19  12:34    <DIR>          .
2016/12/19  12:34    <DIR>          ..
               0 File(s)              0 bytes
               2 Dir(s)  58,419,658,752 bytes free
```

* Initialize project.

```
F:\Soft\Haskell\ccvtest>ccv --init
Reinitialized project in F:\Soft\Haskell\ccvtest
Reinitialized Configuration File.
```

* Set global user name and contact email for the first time. All project created by then will inherit these attributions.

```
F:\Soft\Haskell\ccvtest>ccv --author Kleon
The author has been set to Kleon

F:\Soft\Haskell\ccvtest>ccv --email 1995dingli@gmail.com
The email has been set to 1995dingli@gmail.com
```

* Generate a template Verilog File.

```
F:\Soft\Haskell\ccvtest>ccv -n test
Generated test.v
```

The generated template file is shown as:

<!-- **Verilog Code Block** -->
```Verilog
//////////////////////////////////////////////////
//File    : test.v
//Module  : test
//Author  : Kleon
//Contact : 1995dingli@gmail.com
//Func    : 
//Create  : 2016/12/19
//Version : 0.0.0.1
//////////////////////////////////////////////////

module test
#(
 parameter P = 1
)
(
input  wire  clk,
input  wire  rst
);

endmodule
```


* Create sub-directorys cell and calc.

```
F:\Soft\Haskell\ccvtest>mkdir cell

F:\Soft\Haskell\ccvtest>mkdir calc

F:\Soft\Haskell\ccvtest>dir
 Volume in drive F is Project
 Volume Serial Number is 32C7-6C5E

 Directory of F:\Soft\Haskell\ccvtest

2016/12/19  13:24    <DIR>          .
2016/12/19  13:24    <DIR>          ..
2016/12/19  13:21               714 .ccv.project
2016/12/19  13:24    <DIR>          calc
2016/12/19  13:24    <DIR>          cell
2016/12/19  13:21               351 test.v
               2 File(s)          1,065 bytes
               4 Dir(s)  58,419,109,888 bytes free
```

* Create test cells a, b, and c1. Because c is reserved in Cynide.

```
F:\Soft\Haskell\ccvtest\cell>ccv -n a b c1
Generated a.v
Generated b.v
Generated c1.v
```

* Those files are edited, and become mess.

a.v
```Verilog
//////////////////////////////////////////////////
//File    : a.v
//Module  : a
//Author  : Kleon
//Contact : 1995dingli@gmail.com
//Func    : 
//Create  : 2016/12/19
//Version : 0.0.0.1
//////////////////////////////////////////////////

module a
#(
 parameter W = 8,
 parameter L = 8
)
(
input  wire            clk,
input  wire            rst,
input  [W-1:0]  in,
input  control,
output reg [7:0] out,
output [3:0] state
);

assign state = out[3:0];

always @(posedge clk or posedge rst)
begin
   if (rst)
     begin
       out <= 'b0;
     end
   else if (control)
       out <= in[7:0];
   else out <= in[W-1:W-8];
end

endmodule

```
b.v
```

//////////////////////////////////////////////////
//File    : b.v
//Module  : b
//Author  : Kleon
//Contact : 1995dingli@gmail.com
//Func    : 
//Create  : 2016/12/19
//Version : 0.0.0.1
//////////////////////////////////////////////////

module b
#(
 parameter WIDTH = 4,
 parameter NUM = 4
)
(
input  wire  clk,
input  wire  rst,
input  [NUM-1:0] lanes,
input [WIDTH-1:0] data,
output reg arbitrary
);

always @ (*)
begin
    casex (lanes)
        'b0001 : arbitrary = data[0];
        'b001x : arbitrary = &data[1:0];
        'b01xx : arbitrary = |data[2:0];
        'b1xxx : arbitrary = ~|data[3:0];
        default: arbitrary = 0;
    endcase
end

endmodule

```
c1.v
```Verilog
//////////////////////////////////////////////////
//File    : c1.v
//Module  : c1
//Author  : Kleon
//Contact : 1995dingli@gmail.com
//Func    : 
//Create  : 2016/12/19
//Version : 0.0.0.1
//////////////////////////////////////////////////

module c1
#(
 parameter P = 1
)
(
input  wire  clk,
input  wire  rst,
input cin,
output reg cout
);

always @ (posedge clk) cout <= cin;

endmodule

```

* Formatize those files. The format argument can also be applied to multiple files.

```
F:\Soft\Haskell\ccvtest\cell>ccv -f a.v
Pretty print a.v

F:\Soft\Haskell\ccvtest\cell>ccv -f a.v b.v c1.v
Pretty print a.v
Pretty print b.v
Pretty print c1.v

F:\Soft\Haskell\ccvtest\cell>dir
 Volume in drive F is Project
 Volume Serial Number is 32C7-6C5E

 Directory of F:\Soft\Haskell\ccvtest\cell

2016/12/19  15:04    <DIR>          .
2016/12/19  15:04    <DIR>          ..
2016/12/19  15:04               708 a.v
2016/12/19  15:02               673 a.v.old
2016/12/19  15:04               342 b.v
2016/12/19  14:12               342 b.v.old
2016/12/19  15:04               342 c1.v
2016/12/19  14:12               342 c1.v.old
               6 File(s)          2,749 bytes
               2 Dir(s)  58,418,786,304 bytes free
```

* Create more files. To use IP core, add the port description file Verilog. If there is any change in project, update project with ccv --update or ccv -u.

```
F:\Soft\Haskell\ccvtest\cell>ccv --update
find project root in F:\Soft\Haskell\ccvtest\.ccv.project

F:\Soft\Haskell\ccvtest\cell>ccv --u
find project root in F:\Soft\Haskell\ccvtest\.ccv.project
```

* Use ccv -H to show the hierarchy of project. All those modules include no instances, or intances of them are invisible (IP).

```
F:\Soft\Haskell\ccvtest\cell>ccv -H
|top::test
|top::c1
|top::b
|top::a
|top::toe
|top::nn
|top::dma
```

* Generate top file from the existing modules with Cynide file (.cy). The Cynide file can be anywhere in the project, since ccv scans the whole project to get module declaration.

```
m new {
i clock
i reset

p W 8
p L 4
i in out
i data W
i val W L
i strb W/8
i en
o ready
o out L
o result W L

n a ainst(
clk clock
rst reset
in data
out result
)

n b binst

n b binst2
}
```

* Use ccv -t to generate a top file.

```
F:\Soft\Haskell\ccvtest>ccv -t new.cy
Ready to convert:new.cy
```

* The generated Verilog is shown as:

```
module new
#(
 parameter W = 8,
 parameter L = 4
)
(
input  wire                clock,
input  wire                reset,
input  wire [out - 1:0]    in,
input  wire [W - 1:0]      data,
input  wire [W * L - 1:0]  val,
input  wire [W / 8 - 1:0]  strb,
input  wire                en,
output wire                ready,
output wire [L - 1:0]      out,
output wire [W * L - 1:0]  result
);


a
ainst
(
         .clk(clock),
         .rst(reset),
         .in(data),
         .out(result),
         .control(control),
         .state(state)
);

b
binst
(
         .clk(clk),
         .rst(rst),
         .lanes(lanes),
         .data(data),
         .arbitrary(arbitrary)
);

c1
cinst
(
          .clk(clk),
          .rst(rst),
          .cin(cin),
          .cout(cout)
);
endmodule
```

* Since the port direction can not be telled from the port name of instances, and the "port_o" or "port_i" is ugly. Generate top with -p. If apply -fp on a Verilog file, ccv will generated a new verilog file with port comments.

```
F:\Soft\Haskell\ccvtest>ccv -tp new.cy
Ready to convert:new.cy

F:\Soft\Haskell\ccvtest>ccv -fp new.v
Pretty print new.v
```

* The Verilog with port comments is shown as below. The port comments include port direction and port width. All port with connection specified will connect, otherwise connect to a same name wire.

```
module new
#(
 parameter W = 8,
 parameter L = 4
)
(
input  wire                clock,
input  wire                reset,
input  wire [out - 1:0]    in,
input  wire [W - 1:0]      data,
input  wire [W * L - 1:0]  val,
input  wire [W / 8 - 1:0]  strb,
input  wire                en,
output wire                ready,
output wire [L - 1:0]      out,
output wire [W * L - 1:0]  result
);


a 
#(
     .W(8),
     .L(8)
)
ainst
(
                   //i   (1 * 1)
                   .clk(clock),
                   //i   (1 * 1)
                   .rst(reset),
                   //i   (W * 1)
                   .in(data),
                   //o   (8 * 1)
                   .out(result),
                   //i   (1 * 1)
                   .control(control),
                   //o   (4 * 1)
                   .state(state)
);

b 
#(
     .WIDTH(4),
     .NUM(4)
)
binst
(
                     //i   (1 * 1)
                     .clk(clk),
                     //i   (1 * 1)
                     .rst(rst),
                     //i   (NUM * 1)
                     .lanes(lanes),
                     //i   (WIDTH * 1)
                     .data(data),
                     //o   (1 * 1)
                     .arbitrary(arbitrary)
);

c1 
#(
      .P(1)
)
cinst
(
                    //i   (1 * 1)
                    .clk(clk),
                    //i   (1 * 1)
                    .rst(rst),
                    //i   (1 * 1)
                    .cin(cin),
                    //o   (1 * 1)
                    .cout(cout)
);
endmodule
```

* We may want to instantiate more instances in new.v. And ccv -tip can help.

```
F:\Soft\Haskell\ccvtest>ccv -tip new.v add.cy
Add add.cy into new.v
```

```
m _ 
{
c result ready cin
n a ainst2
}

m _
{
c lanes out result[1]
}
```


```Verilog
module new
#(
 parameter W = 8,
 parameter L = 4
)
(
input  wire                clock,
input  wire                reset,
input  wire [out - 1:0]    in,
input  wire [W - 1:0]      data,
input  wire [W * L - 1:0]  val,
input  wire [W / 8 - 1:0]  strb,
input  wire                en,
output wire                ready,
output wire [L - 1:0]      out,
output wire [W * L - 1:0]  result
);


a 
#(
     .W(8),
     .L(8)
)
ainst
(
                   //i   (1 * 1)
                   .clk(clock),
                   //i   (1 * 1)
                   .rst(reset),
                   //i   (W * 1)
                   .in(data),
                   //o   (8 * 1)
                   .out(result),
                   //i   (1 * 1)
                   .control(control),
                   //o   (4 * 1)
                   .state(state)
);

b 
#(
     .WIDTH(4),
     .NUM(4)
)
binst
(
                     //i   (1 * 1)
                     .clk(clk),
                     //i   (1 * 1)
                     .rst(rst),
                     //i   (NUM * 1)
                     .lanes(lanes),
                     //i   (WIDTH * 1)
                     .data(data),
                     //o   (1 * 1)
                     .arbitrary(arbitrary)
);

c1 
#(
      .P(1)
)
cinst
(
                    //i   (1 * 1)
                    .clk(clk),
                    //i   (1 * 1)
                    .rst(rst),
                    //i   (1 * 1)
                    .cin(cin),
                    //o   (1 * 1)
                    .cout(cout)
);
assign result = ready;
assign result = cin;

a 
#(
     .W(8),
     .L(8)
)
ainst2
(
                    //i   (1 * 1)
                    .clk(clk),
                    //i   (1 * 1)
                    .rst(rst),
                    //i   (W * 1)
                    .in(in),
                    //i   (1 * 1)
                    .control(control),
                    //o   (8 * 1)
                    .out(out),
                    //o   (4 * 1)
                    .state(state)
);
assign lanes = out;
assign lanes = result[1];
endmodule
```

* Since the a, b and c1 is small module, they can be merged into one file.

```
F:\Soft\Haskell\ccvtest\cell>ccv -m a.v b.v c1.v
b.v c1.v are merged into a.v
```

* Sometimes we want to merge serveral modules into one.

```
F:\Soft\Haskell\ccvtest>ccv -mi test.v new.v
new.v is merged into test.v
```

* The merged file is shown as below.

```Verilog
module test
#(
 parameter P = 1,
 parameter W = 8,
 parameter L = 4
)
(
input  wire                clk,
input  wire                rst,
input  wire                clock,
input  wire                reset,
input  wire [out - 1:0]    in,
input  wire [W - 1:0]      data,
input  wire [W * L - 1:0]  val,
input  wire [W / 8 - 1:0]  strb,
input  wire                en,
output wire                ready,
output wire [L - 1:0]      out,
output wire [W * L - 1:0]  result
);


a 
#(
     .W(8),
     .L(8)
)
ainst
(
                   .clk(clock),
                   .rst(reset),
                   .in(data),
                   .out(result),
                   .control(control),
                   .state(state)
);

b 
#(
     .WIDTH(4),
     .NUM(4)
)
binst
(
                     .clk(clk),
                     .rst(rst),
                     .lanes(lanes),
                     .data(data),
                     .arbitrary(arbitrary)
);

c1 
#(
      .P(1)
)
cinst
(
                    .clk(clk),
                    .rst(rst),
                    .cin(cin),
                    .cout(cout)
);
endmodule
```

* Modules in one file can be split into serval files.

```
F:\Soft\Haskell\ccvtest\cell>ccv -s a.v
a.v is split into a.v b.v c1.v
```

* For larger IP, ccv can be even more useful.

A neural network IP is shown as:

```Verilog
module nn
#(
 parameter DATA_WIDTH = 64,
 parameter AXI_WIDTH = 32
)
(
input  user_clk,
input  app_clk,
input  global_reset,
input  axi_resetn,
input [DATA_WIDTH-1:0] data_in,
input  valid_in,
input [DATA_WIDTH/8-1:0] keep_in,
input last_in,
output ready_in,
output [DATA_WIDTH-1:0] data_out,
output  valid_out,
output [DATA_WIDTH/8-1:0] keep_out,
output last_out,
input ready_out,
input [3:0] axi_awid,
input [3:0] axi_awlen,
input [2:0] axi_awsize,
input [1:0] axi_awburst,
input [3:0] axi_awlock,
input [3:0] axi_awcache,
input [3:0] axi_awprot,
input [31:0] axi_awaddr,
input axi_awvalid,
output axi_awready,
input [AXI_WIDTH-1:0] axi_wdata,
output axi_wready,
input axi_wvalid,
input [AXI_WIDTH/8-1:0] axi_wstrb,
input axi_wlast,
output [1:0] axi_bresp,
output axi_bvalid,
input axi_bready,
input [3:0] axi_arid,
input [3:0] axi_arlen,
input [2:0] axi_arsize,
input [1:0] axi_arburst,
input [3:0] axi_arlock,
input [3:0] axi_arcache,
input [3:0] axi_arprot,
input [31:0] axi_araddr,
input axi_arvalid,
output axi_arready,
output [AXI_WIDTH-1:0] axi_rdata,
output axi_rvalid,
output [AXI_WIDTH/8-1:0] axi_rstrb,
input axi_rready
);

endmodule
```

* The port declaration is really a mess. It can also be formatized.

```
//////////////////////////////////////////////////
//File    : nn.v
//Module  : nn
//Author  : Kleon
//Contact : 1995dingli@gmail.com
//Func    : 
//Create  : 2016/12/19
//Version : 0.0.0.1
//////////////////////////////////////////////////

module nn
#(
 parameter DATA_WIDTH = 64,
 parameter AXI_WIDTH = 32
)
(
input  wire                         user_clk,
input  wire                         app_clk,
input  wire                         global_reset,
input  wire                         axi_resetn,
input  wire [DATA_WIDTH - 1:0]      data_in,
input  wire                         valid_in,
input  wire [DATA_WIDTH / 8 - 1:0]  keep_in,
input  wire                         last_in,
output wire                         ready_in,
output wire [DATA_WIDTH - 1:0]      data_out,
output wire                         valid_out,
output wire [DATA_WIDTH / 8 - 1:0]  keep_out,
output wire                         last_out,
input  wire                         ready_out,
input  wire [3:0]                   axi_awid,
input  wire [3:0]                   axi_awlen,
input  wire [2:0]                   axi_awsize,
input  wire [1:0]                   axi_awburst,
input  wire [3:0]                   axi_awlock,
input  wire [3:0]                   axi_awcache,
input  wire [3:0]                   axi_awprot,
input  wire [31:0]                  axi_awaddr,
input  wire                         axi_awvalid,
output wire                         axi_awready,
input  wire [AXI_WIDTH - 1:0]       axi_wdata,
output wire                         axi_wready,
input  wire                         axi_wvalid,
input  wire [AXI_WIDTH / 8 - 1:0]   axi_wstrb,
input  wire                         axi_wlast,
output wire [1:0]                   axi_bresp,
output wire                         axi_bvalid,
input  wire                         axi_bready,
input  wire [3:0]                   axi_arid,
input  wire [3:0]                   axi_arlen,
input  wire [2:0]                   axi_arsize,
input  wire [1:0]                   axi_arburst,
input  wire [3:0]                   axi_arlock,
input  wire [3:0]                   axi_arcache,
input  wire [3:0]                   axi_arprot,
input  wire [31:0]                  axi_araddr,
input  wire                         axi_arvalid,
output wire                         axi_arready,
output wire [AXI_WIDTH - 1:0]       axi_rdata,
output wire                         axi_rvalid,
output wire [AXI_WIDTH / 8 - 1:0]   axi_rstrb,
input  wire                         axi_rready
);

endmodule
```

* We have other two IP dma and toe. dma sends data from pcie, toe packtizes data and sends it via network. With ccv -tp, we can immediately get those instances.

```
F:\Soft\Haskell\ccvtest>ccv -tp app.cy
Ready to convert:app.cy
```

```
m app
{
p DATA_WIDTH 64
p AXI_WIDTH 64
i clk250
i clk100

n dma dma_i
n toe toe_i
n nn nn_i
}
```

```Verilog
module app
#(
 parameter DATA_WIDTH = 64,
 parameter AXI_WIDTH = 64
)
(
input  wire  clk250,
input  wire  clk100
);


dma 
#(
       .CORE_BE_WIDTH(32),
       .KEEP_WIDTH(32),
       .CORE_DATA_WIDTH(256)
)
dma_i
(
                                     //i   (1 * 1)
                                     .user_reset(user_reset),
                                     //i   (1 * 1)
                                     .user_clk(user_clk),
                                     //i   (1 * 1)
                                     .user_lnk_up(user_lnk_up),
                                     //i   (8 * 1)
                                     .clk_period_in_ns(clk_period_in_ns),
                                     //i   (1 * 1)
                                     .user_interrupt(user_interrupt),
                                     //o   (1 * 1)
                                     .s_axis_rq_tlast(s_axis_rq_tlast),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .s_axis_rq_tdata(s_axis_rq_tdata),
                                     //o   (60 * 1)
                                     .s_axis_rq_tuser(s_axis_rq_tuser),
                                     //o   (KEEP_WIDTH * 1)
                                     .s_axis_rq_tkeep(s_axis_rq_tkeep),
                                     //i   (4 * 1)
                                     .s_axis_rq_tready(s_axis_rq_tready),
                                     //o   (1 * 1)
                                     .s_axis_rq_tvalid(s_axis_rq_tvalid),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .m_axis_rc_tdata(m_axis_rc_tdata),
                                     //i   (75 * 1)
                                     .m_axis_rc_tuser(m_axis_rc_tuser),
                                     //i   (1 * 1)
                                     .m_axis_rc_tlast(m_axis_rc_tlast),
                                     //i   (KEEP_WIDTH * 1)
                                     .m_axis_rc_tkeep(m_axis_rc_tkeep),
                                     //i   (1 * 1)
                                     .m_axis_rc_tvalid(m_axis_rc_tvalid),
                                     //o   (1 * 1)
                                     .m_axis_rc_tready(m_axis_rc_tready),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .m_axis_cq_tdata(m_axis_cq_tdata),
                                     //i   (85 * 1)
                                     .m_axis_cq_tuser(m_axis_cq_tuser),
                                     //i   (1 * 1)
                                     .m_axis_cq_tlast(m_axis_cq_tlast),
                                     //i   (KEEP_WIDTH * 1)
                                     .m_axis_cq_tkeep(m_axis_cq_tkeep),
                                     //i   (1 * 1)
                                     .m_axis_cq_tvalid(m_axis_cq_tvalid),
                                     //o   (1 * 1)
                                     .m_axis_cq_tready(m_axis_cq_tready),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .s_axis_cc_tdata(s_axis_cc_tdata),
                                     //o   (33 * 1)
                                     .s_axis_cc_tuser(s_axis_cc_tuser),
                                     //o   (1 * 1)
                                     .s_axis_cc_tlast(s_axis_cc_tlast),
                                     //o   (KEEP_WIDTH * 1)
                                     .s_axis_cc_tkeep(s_axis_cc_tkeep),
                                     //o   (1 * 1)
                                     .s_axis_cc_tvalid(s_axis_cc_tvalid),
                                     //i   (1 * 1)
                                     .s_axis_cc_tready(s_axis_cc_tready),
                                     //i   (12 * 1)
                                     .fc_cpld(fc_cpld),
                                     //i   (8 * 1)
                                     .fc_cplh(fc_cplh),
                                     //i   (12 * 1)
                                     .fc_npd(fc_npd),
                                     //i   (8 * 1)
                                     .fc_nph(fc_nph),
                                     //i   (12 * 1)
                                     .fc_pd(fc_pd),
                                     //i   (8 * 1)
                                     .fc_ph(fc_ph),
                                     //i   (3 * 1)
                                     .fc_sel(fc_sel),
                                     //o   (1 * 1)
                                     .cfg_err_cor(cfg_err_cor),
                                     //o   (1 * 1)
                                     .cfg_err_ur(cfg_err_ur),
                                     //o   (1 * 1)
                                     .cfg_err_ecrc(cfg_err_ecrc),
                                     //o   (1 * 1)
                                     .cfg_err_cpl_timeout(cfg_err_cpl_timeout),
                                     //o   (1 * 1)
                                     .cfg_err_cpl_abort(cfg_err_cpl_abort),
                                     //o   (1 * 1)
                                     .cfg_err_cpl_unexpect(cfg_err_cpl_unexpect),
                                     //o   (1 * 1)
                                     .cfg_err_posted(cfg_err_posted),
                                     //o   (1 * 1)
                                     .cfg_err_locked(cfg_err_locked),
                                     //o   (48 * 1)
                                     .cfg_err_tlp_cpl_header(cfg_err_tlp_cpl_header),
                                     //o   (4 * 1)
                                     .cfg_interrupt_int(cfg_interrupt_int),
                                     //i   (1 * 1)
                                     .cfg_interrupt_sent(cfg_interrupt_sent),
                                     //i   (1 * 1)
                                     .cfg_interrupt_msi_sent(cfg_interrupt_msi_sent),
                                     //o   (1 * 1)
                                     .cfg_interrupt_msi_int(cfg_interrupt_msi_int),
                                     //i   (2 * 1)
                                     .cfg_interrupt_msi_enable(cfg_interrupt_msi_enable),
                                     //i   (2 * 1)
                                     .cfg_interrupt_msix_enable(cfg_interrupt_msix_enable),
                                     //i   (1 * 1)
                                     .cfg_interrupt_msixfm(cfg_interrupt_msixfm),
                                     //o   (1 * 1)
                                     .cfg_turnoff_ok(cfg_turnoff_ok),
                                     //i   (1 * 1)
                                     .cfg_to_turnoff(cfg_to_turnoff),
                                     //o   (1 * 1)
                                     .cfg_trn_pending(cfg_trn_pending),
                                     //o   (1 * 1)
                                     .cfg_pm_wake(cfg_pm_wake),
                                     //i   (8 * 1)
                                     .cfg_function_status(cfg_function_status),
                                     //i   (3 * 1)
                                     .cfg_max_payload(cfg_max_payload),
                                     //i   (3 * 1)
                                     .cfg_max_read_req(cfg_max_read_req),
                                     //i   (1 * 1)
                                     .c2s0_aclk(c2s0_aclk),
                                     //i   (1 * 1)
                                     .c2s0_tlast(c2s0_tlast),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .c2s0_tdata(c2s0_tdata),
                                     //i   (CORE_BE_WIDTH * 1)
                                     .c2s0_tkeep(c2s0_tkeep),
                                     //i   (1 * 1)
                                     .c2s0_tvalid(c2s0_tvalid),
                                     //o   (1 * 1)
                                     .c2s0_tready(c2s0_tready),
                                     //i   (64 * 1)
                                     .c2s0_tuser(c2s0_tuser),
                                     //o   (1 * 1)
                                     .c2s0_areset_n(c2s0_areset_n),
                                     //i   (1 * 1)
                                     .c2s1_aclk(c2s1_aclk),
                                     //i   (1 * 1)
                                     .c2s1_tlast(c2s1_tlast),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .c2s1_tdata(c2s1_tdata),
                                     //i   (CORE_BE_WIDTH * 1)
                                     .c2s1_tkeep(c2s1_tkeep),
                                     //i   (1 * 1)
                                     .c2s1_tvalid(c2s1_tvalid),
                                     //o   (1 * 1)
                                     .c2s1_tready(c2s1_tready),
                                     //i   (64 * 1)
                                     .c2s1_tuser(c2s1_tuser),
                                     //o   (1 * 1)
                                     .c2s1_areset_n(c2s1_areset_n),
                                     //i   (1 * 1)
                                     .c2s2_aclk(c2s2_aclk),
                                     //i   (1 * 1)
                                     .c2s2_tlast(c2s2_tlast),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .c2s2_tdata(c2s2_tdata),
                                     //i   (CORE_BE_WIDTH * 1)
                                     .c2s2_tkeep(c2s2_tkeep),
                                     //i   (1 * 1)
                                     .c2s2_tvalid(c2s2_tvalid),
                                     //o   (1 * 1)
                                     .c2s2_tready(c2s2_tready),
                                     //i   (64 * 1)
                                     .c2s2_tuser(c2s2_tuser),
                                     //o   (1 * 1)
                                     .c2s2_areset_n(c2s2_areset_n),
                                     //i   (1 * 1)
                                     .c2s3_aclk(c2s3_aclk),
                                     //i   (1 * 1)
                                     .c2s3_tlast(c2s3_tlast),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .c2s3_tdata(c2s3_tdata),
                                     //i   (CORE_BE_WIDTH * 1)
                                     .c2s3_tkeep(c2s3_tkeep),
                                     //i   (1 * 1)
                                     .c2s3_tvalid(c2s3_tvalid),
                                     //o   (1 * 1)
                                     .c2s3_tready(c2s3_tready),
                                     //i   (64 * 1)
                                     .c2s3_tuser(c2s3_tuser),
                                     //o   (1 * 1)
                                     .c2s3_areset_n(c2s3_areset_n),
                                     //i   (1 * 1)
                                     .s2c0_aclk(s2c0_aclk),
                                     //o   (1 * 1)
                                     .s2c0_tlast(s2c0_tlast),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .s2c0_tdata(s2c0_tdata),
                                     //o   (CORE_BE_WIDTH * 1)
                                     .s2c0_tkeep(s2c0_tkeep),
                                     //o   (1 * 1)
                                     .s2c0_tvalid(s2c0_tvalid),
                                     //i   (1 * 1)
                                     .s2c0_tready(s2c0_tready),
                                     //o   (64 * 1)
                                     .s2c0_tuser(s2c0_tuser),
                                     //o   (1 * 1)
                                     .s2c0_areset_n(s2c0_areset_n),
                                     //i   (1 * 1)
                                     .s2c1_aclk(s2c1_aclk),
                                     //o   (1 * 1)
                                     .s2c1_tlast(s2c1_tlast),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .s2c1_tdata(s2c1_tdata),
                                     //o   (CORE_BE_WIDTH * 1)
                                     .s2c1_tkeep(s2c1_tkeep),
                                     //o   (1 * 1)
                                     .s2c1_tvalid(s2c1_tvalid),
                                     //i   (1 * 1)
                                     .s2c1_tready(s2c1_tready),
                                     //o   (64 * 1)
                                     .s2c1_tuser(s2c1_tuser),
                                     //o   (1 * 1)
                                     .s2c1_areset_n(s2c1_areset_n),
                                     //i   (1 * 1)
                                     .s2c2_aclk(s2c2_aclk),
                                     //o   (1 * 1)
                                     .s2c2_tlast(s2c2_tlast),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .s2c2_tdata(s2c2_tdata),
                                     //o   (CORE_BE_WIDTH * 1)
                                     .s2c2_tkeep(s2c2_tkeep),
                                     //o   (1 * 1)
                                     .s2c2_tvalid(s2c2_tvalid),
                                     //i   (1 * 1)
                                     .s2c2_tready(s2c2_tready),
                                     //o   (64 * 1)
                                     .s2c2_tuser(s2c2_tuser),
                                     //o   (1 * 1)
                                     .s2c2_areset_n(s2c2_areset_n),
                                     //i   (1 * 1)
                                     .s2c3_aclk(s2c3_aclk),
                                     //o   (1 * 1)
                                     .s2c3_tlast(s2c3_tlast),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .s2c3_tdata(s2c3_tdata),
                                     //o   (CORE_BE_WIDTH * 1)
                                     .s2c3_tkeep(s2c3_tkeep),
                                     //o   (1 * 1)
                                     .s2c3_tvalid(s2c3_tvalid),
                                     //i   (1 * 1)
                                     .s2c3_tready(s2c3_tready),
                                     //o   (64 * 1)
                                     .s2c3_tuser(s2c3_tuser),
                                     //o   (1 * 1)
                                     .s2c3_areset_n(s2c3_areset_n),
                                     //i   (1 * 1)
                                     .t_areset_n(t_areset_n),
                                     //i   (1 * 1)
                                     .t_aclk(t_aclk),
                                     //o   (1 * 1)
                                     .t_awvalid(t_awvalid),
                                     //i   (1 * 1)
                                     .t_awready(t_awready),
                                     //o   (32 * 1)
                                     .t_awaddr(t_awaddr),
                                     //o   (4 * 1)
                                     .t_awlen(t_awlen),
                                     //o   (3 * 1)
                                     .t_awregion(t_awregion),
                                     //o   (3 * 1)
                                     .t_awsize(t_awsize),
                                     //o   (1 * 1)
                                     .t_wvalid(t_wvalid),
                                     //i   (1 * 1)
                                     .t_wready(t_wready),
                                     //o   (CORE_DATA_WIDTH * 1)
                                     .t_wdata(t_wdata),
                                     //o   (CORE_BE_WIDTH * 1)
                                     .t_wstrb(t_wstrb),
                                     //o   (1 * 1)
                                     .t_wlast(t_wlast),
                                     //i   (1 * 1)
                                     .t_bvalid(t_bvalid),
                                     //o   (1 * 1)
                                     .t_bready(t_bready),
                                     //i   (2 * 1)
                                     .t_bresp(t_bresp),
                                     //o   (1 * 1)
                                     .t_arvalid(t_arvalid),
                                     //i   (1 * 1)
                                     .t_arready(t_arready),
                                     //o   (32 * 1)
                                     .t_araddr(t_araddr),
                                     //o   (4 * 1)
                                     .t_arlen(t_arlen),
                                     //o   (3 * 1)
                                     .t_arregion(t_arregion),
                                     //o   (3 * 1)
                                     .t_arsize(t_arsize),
                                     //i   (1 * 1)
                                     .t_rvalid(t_rvalid),
                                     //o   (1 * 1)
                                     .t_rready(t_rready),
                                     //i   (CORE_DATA_WIDTH * 1)
                                     .t_rdata(t_rdata),
                                     //i   (2 * 1)
                                     .t_rresp(t_rresp),
                                     //i   (1 * 1)
                                     .t_rlast(t_rlast)
);

toe 
#(
       .GROUPSIZE(16)
)
toe_i
(
                              //i   (1 * 1)
                              .clk(clk),
                              //i   (1 * 1)
                              .rst(rst),
                              //i   (1 * 1)
                              .s_axi_aclk(s_axi_aclk),
                              //i   (1 * 1)
                              .s_axi_aresetn(s_axi_aresetn),
                              //i   (32 * 1)
                              .s_axi_awaddr(s_axi_awaddr),
                              //i   (1 * 1)
                              .s_axi_awvalid(s_axi_awvalid),
                              //o   (1 * 1)
                              .s_axi_awready(s_axi_awready),
                              //i   (32 * 1)
                              .s_axi_wdata(s_axi_wdata),
                              //i   (4 * 1)
                              .s_axi_wstrb(s_axi_wstrb),
                              //i   (1 * 1)
                              .s_axi_wvalid(s_axi_wvalid),
                              //o   (1 * 1)
                              .s_axi_wready(s_axi_wready),
                              //o   (2 * 1)
                              .s_axi_bresp(s_axi_bresp),
                              //o   (1 * 1)
                              .s_axi_bvalid(s_axi_bvalid),
                              //i   (1 * 1)
                              .s_axi_bready(s_axi_bready),
                              //i   (32 * 1)
                              .s_axi_araddr(s_axi_araddr),
                              //i   (1 * 1)
                              .s_axi_arvalid(s_axi_arvalid),
                              //o   (1 * 1)
                              .s_axi_arready(s_axi_arready),
                              //o   (32 * 1)
                              .s_axi_rdata(s_axi_rdata),
                              //o   (2 * 1)
                              .s_axi_rresp(s_axi_rresp),
                              //o   (1 * 1)
                              .s_axi_rvalid(s_axi_rvalid),
                              //i   (1 * 1)
                              .s_axi_rready(s_axi_rready),
                              //i   (64 * 1)
                              .toe_rx_in_tdata(toe_rx_in_tdata),
                              //i   (8 * 1)
                              .toe_rx_in_tkeep(toe_rx_in_tkeep),
                              //i   (1 * 1)
                              .toe_rx_in_tvalid(toe_rx_in_tvalid),
                              //i   (1 * 1)
                              .toe_rx_in_tlast(toe_rx_in_tlast),
                              //i   (1 * 1)
                              .toe_rx_in_tuser(toe_rx_in_tuser),
                              //o   (16 * 1)
                              .toe_rx_out_tlen(toe_rx_out_tlen),
                              //o   (16 * 1)
                              .toe_rx_out_tuser(toe_rx_out_tuser),
                              //o   (32 * 1)
                              .toe_rx_out_tport(toe_rx_out_tport),
                              //o   (64 * 1)
                              .toe_rx_out_tdata(toe_rx_out_tdata),
                              //o   (8 * 1)
                              .toe_rx_out_tkeep(toe_rx_out_tkeep),
                              //o   (1 * 1)
                              .toe_rx_out_tvalid(toe_rx_out_tvalid),
                              //o   (1 * 1)
                              .toe_rx_out_tlast(toe_rx_out_tlast),
                              //i   (1 * 1)
                              .toe_rx_out_tready(toe_rx_out_tready),
                              //i   (64 * 1)
                              .toe_tx_in_tdata(toe_tx_in_tdata),
                              //i   (8 * 1)
                              .toe_tx_in_tkeep(toe_tx_in_tkeep),
                              //i   (1 * 1)
                              .toe_tx_in_tvalid(toe_tx_in_tvalid),
                              //i   (1 * 1)
                              .toe_tx_in_tlast(toe_tx_in_tlast),
                              //i   (16 * 1)
                              .toe_tx_in_tuser(toe_tx_in_tuser),
                              //i   (16 * 1)
                              .toe_tx_in_tlen(toe_tx_in_tlen),
                              //o   (1 * 1)
                              .toe_tx_in_tready(toe_tx_in_tready),
                              //o   (64 * 1)
                              .toe_tx_out_tdata(toe_tx_out_tdata),
                              //o   (8 * 1)
                              .toe_tx_out_tkeep(toe_tx_out_tkeep),
                              //o   (1 * 1)
                              .toe_tx_out_tvalid(toe_tx_out_tvalid),
                              //o   (1 * 1)
                              .toe_tx_out_tlast(toe_tx_out_tlast),
                              //i   (1 * 1)
                              .toe_tx_out_tready(toe_tx_out_tready)
);

nn 
#(
      .DATA_WIDTH(64),
      .AXI_WIDTH(32)
)
nn_i
(
                            //i   (1 * 1)
                            .user_clk(user_clk),
                            //i   (1 * 1)
                            .app_clk(app_clk),
                            //i   (1 * 1)
                            .global_reset(global_reset),
                            //i   (1 * 1)
                            .axi_resetn(axi_resetn),
                            //i   (DATA_WIDTH * 1)
                            .data_in(data_in),
                            //i   (1 * 1)
                            .valid_in(valid_in),
                            //i   (DATA_WIDTH / 8 * 1)
                            .keep_in(keep_in),
                            //i   (1 * 1)
                            .last_in(last_in),
                            //o   (1 * 1)
                            .ready_in(ready_in),
                            //o   (DATA_WIDTH * 1)
                            .data_out(data_out),
                            //o   (1 * 1)
                            .valid_out(valid_out),
                            //o   (DATA_WIDTH / 8 * 1)
                            .keep_out(keep_out),
                            //o   (1 * 1)
                            .last_out(last_out),
                            //i   (1 * 1)
                            .ready_out(ready_out),
                            //i   (4 * 1)
                            .axi_awid(axi_awid),
                            //i   (4 * 1)
                            .axi_awlen(axi_awlen),
                            //i   (3 * 1)
                            .axi_awsize(axi_awsize),
                            //i   (2 * 1)
                            .axi_awburst(axi_awburst),
                            //i   (4 * 1)
                            .axi_awlock(axi_awlock),
                            //i   (4 * 1)
                            .axi_awcache(axi_awcache),
                            //i   (4 * 1)
                            .axi_awprot(axi_awprot),
                            //i   (32 * 1)
                            .axi_awaddr(axi_awaddr),
                            //i   (1 * 1)
                            .axi_awvalid(axi_awvalid),
                            //o   (1 * 1)
                            .axi_awready(axi_awready),
                            //i   (AXI_WIDTH * 1)
                            .axi_wdata(axi_wdata),
                            //o   (1 * 1)
                            .axi_wready(axi_wready),
                            //i   (1 * 1)
                            .axi_wvalid(axi_wvalid),
                            //i   (AXI_WIDTH / 8 * 1)
                            .axi_wstrb(axi_wstrb),
                            //i   (1 * 1)
                            .axi_wlast(axi_wlast),
                            //o   (2 * 1)
                            .axi_bresp(axi_bresp),
                            //o   (1 * 1)
                            .axi_bvalid(axi_bvalid),
                            //i   (1 * 1)
                            .axi_bready(axi_bready),
                            //i   (4 * 1)
                            .axi_arid(axi_arid),
                            //i   (4 * 1)
                            .axi_arlen(axi_arlen),
                            //i   (3 * 1)
                            .axi_arsize(axi_arsize),
                            //i   (2 * 1)
                            .axi_arburst(axi_arburst),
                            //i   (4 * 1)
                            .axi_arlock(axi_arlock),
                            //i   (4 * 1)
                            .axi_arcache(axi_arcache),
                            //i   (4 * 1)
                            .axi_arprot(axi_arprot),
                            //i   (32 * 1)
                            .axi_araddr(axi_araddr),
                            //i   (1 * 1)
                            .axi_arvalid(axi_arvalid),
                            //o   (1 * 1)
                            .axi_arready(axi_arready),
                            //o   (AXI_WIDTH * 1)
                            .axi_rdata(axi_rdata),
                            //o   (1 * 1)
                            .axi_rvalid(axi_rvalid),
                            //o   (AXI_WIDTH / 8 * 1)
                            .axi_rstrb(axi_rstrb),
                            //i   (1 * 1)
                            .axi_rready(axi_rready)
);
endmodule

```

* After several operations, we can print the current project hierarchy out.

```
F:\Soft\Haskell\ccvtest>ccv -u
find project root in F:\Soft\Haskell\ccvtest\.ccv.project

F:\Soft\Haskell\ccvtest>ccv -H
|top::test
    |a::a
    |b::b
    |c1::c1
|top::new
    |a::a
    |b::b
    |c1::c1
|top::app
    |dma::dma
    |toe::toe
    |nn::nn
```

###Cynide & Verilog

* Cynide is the description file for ccv. Convenient Y N I DEscription... 

It is implemented in:
```
import Language.Cynide
```

It is defined as:

```Haskell
//Cynide
//[] for list, | for or, <> for maybe, {} and () for key symbol
[
    m modulename 
    {
      [
         p parameter
        |i input <length> <width>
        |o output <length> <width>
        |io inout <length> <width>
        |c source [drains]
        |n instance 
        (
            [para value |pin net]
        )
      ]  
    }
]
```

* Verilog adpots Verilog-2001 standard, which is implemented in :

```
import Language.Verilog
```

Add verilog 2001 features to the original Verilog parser.
```
ASNI C style port declaration
ASNI C style optional parameter declaration
generate for statement
generate if statement
localparam statement
```

* TODO
```
comments inherit
macro statement
```


###Error Feedback
All kinds of error feedback.

* File parsec provided error.

```
F:\Soft\Haskell\ccvtest>ccv -u
find project root in F:\Soft\Haskell\ccvtest\.ccv.project
ccv: Fail1: F:\Soft\Haskell\ccvtest\new.v
"F:\Soft\Haskell\ccvtest\new.v" (line 14, column 1):
unexpected "o"
expecting "," or ")"

CallStack (from HasCallStack):
  error, called at .\VerilogHandle.hs:70:24 in main:VerilogHandle
```

```
F:\Soft\Haskell\ccvtest>ccv -t new.cy
Ready to convert:new.cy
Fail1: F:\Soft\Haskell\ccvtest\new.cy
"F:\Soft\Haskell\ccvtest\new.cy" (line 2, column 1):
unexpected "i"
expecting "{"

ccv: Maybe.fromJust: Nothing
```

* Module name confliction

```
F:\Soft\Haskell\ccvtest>ccv --init
Reinitialized project in F:\Soft\Haskell\ccvtest
Name conflict: app::F:\Soft\Haskell\ccvtest\app.v == app::F:\Soft\Haskell\ccvtest\app - Copy.v
Name conflict: app::F:\Soft\Haskell\ccvtest\app - Copy.v == app::F:\Soft\Haskell\ccvtest\app.v
```

* Port missing

```
F:\Soft\Haskell\ccvtest>ccv -t new.cy
Ready to convert:new.cy
ccv: port is not in a::F:\Soft\Haskell\ccvtest\cell\a.v
CallStack (from HasCallStack):
  error, called at .\VerilogHandle.hs:272:30 in main:VerilogHandle
```


* File argument missing

```
F:\Soft\Haskell\ccvtest>ccv -tip new.v
ccv: No cynide file specified.
CallStack (from HasCallStack):
  error, called at .\ProjectHandle.hs:140:27 in main:ProjectHandle

F:\Soft\Haskell\ccvtest>ccv -m a b c
ccv: No verilog file specified. Add extension .v
CallStack (from HasCallStack):
  error, called at .\ProjectHandle.hs:314:29 in main:ProjectHandle

F:\Soft\Haskell\ccvtest>ccv -s
ccv: No verilog file specified. Add extension .v
CallStack (from HasCallStack):
  error, called at .\ProjectHandle.hs:331:29 in main:ProjectHandle

F:\Soft\Haskell\ccvtest>ccv -m new.v
ccv: Only one file specified
CallStack (from HasCallStack):
  error, called at .\ProjectHandle.hs:315:37 in main:ProjectHandle
```

* Unexpected file content

```
F:\Soft\Haskell\ccvtest>ccv -s new.v
ccv: Original file empty.
CallStack (from HasCallStack):
  error, called at .\VerilogHandle.hs:360:33 in main:VerilogHandle
```

* Unexpected project file change.

```
F:\Soft\Haskell\ccvtest>ccv -t new.cy
Ready to convert:new.cy
ccv: Project file corrupted. Try ccv --init or ccv --update
CallStack (from HasCallStack):
  error, called at .\ProjectHandle.hs:188:22 in main:ProjectHandle
```

* Excute ccv out of project folder.

```
F:\Soft\Haskell>ccv -t new.cy
Ready to convert:new.cy
ccv: Project File is not found. Init project first. (ccv --init)
CallStack (from HasCallStack):
  error, called at .\ProjectHandle.hs:198:37 in main:ProjectHandle
```

###Dependency

```Haskell
-- Directory search
-- cabal install directory-Tree
import Data.Aeson (decode, encode)

-- Json parsing and print
-- cabal install aeson
import System.Directory.Tree

-- Argument parsing
import System.Console.GetOpt
import System.Environment

-- Parsec
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr

-- Pretty Print
import Text.PrettyPrint

-- Directory
import System.FilePath
import System.Directory

-- Time
import Data.Time.Clock
import Data.Time.Calendar
```


---
The rest of this file is the old design document.

The template verilog

```Verilog
//
//Filename : filename
//Author   : authorname
//Contact  : emailaddress
//Function : ...reserved for author
//Hierarchy: cell
//Revision :
//v0.1     : reserved for author
//

`timescale 1ps/1ps
module filename #(
)
(
    input  wire             clk,
    input  wire             rst
);

endmodule
```

A typical ccv(Convenient Coding for Verilog) file is shown below:
```Verilog
module topfilename // same as the filename by default
//the keyword module is preserved
//parameter declaration
//must on the top, the keyword p is preserved
p WIDTH 2
p ADDRESS 16'hA000
    
//port declaration
//dummyport portname portwidth memportwidth
//the keyword clk[], rst[], i, o is preserved
clk deviceclk
rst devicerst
i a 32
i b 32
i c 32
o d 32
o e 32
o f 32
o para WIDTH 9

//port connection
//connect to port
d result[0]
e result[1]
f result[2]

//module instantiation
moduleA nameA
    clk usrclk
    ~rst nreset

moduleB nameB
    alpha port1
    beta port2
    gama port3

//Automatically generate a memtype wire
//Descripted in normal wire
moduleC nameCalpha
    alpha in
    signal[0] out

moduleC nameCbeta
    alpha in
    signal[1] out

moduleC nameCgama
    alpha in
    signal[2] out

//generate duplicated instance all expanded
//the keyword duplicate is preserved
duplicate moduleD nameD 3 nogen
    clk calcclk
    rst calcrst
    signal[] in[]
    result[] out[]

//generate duplicated instance with gen grammar
duplicate moduleE nameE 9 gen
    clk clk
    ~rst nrst
    para[] paravec

```

The corresponding Coverted Verilog file is shown below:
```Verilog
//
//Filename : topfilename
//Author   : authorname
//Contact  : emailaddress
//Function : ...reserved for author
//Hierarchy: top
//Revision :
//v0.1     : reserved for author
//

//`timescale 1ps/1ps
// same as the filename by default
//the keyword module is preserved
//parameter declaration
//must on the top, the keyword p is preserved
module topfilename #(
parameter WIDTH = 32,
parameter ADDRESS = 16'hA000
)
(
//port declaration
//dummyport portname portwidth memportwidth
//the keyword clk[], rst[], i, o is preserved
input  wire                 clk,
input  wire                 rst,
input  wire [31:0]          a,
input  wire [31:0]          b,
input  wire [31:0]          c,
output wire [31:0]          d,
output wire [31:0]          e,
output wire [31:0]          f,
output wire [WIDTH*9-1:0]   para,
output wire                 feedback
);

//Wire declaration
wire [31:0]     alpha;
wire [31:0]     beta;
wire [31:0]     gama;
wire [32*3-1:0] result;
wire [32*3-1:0] signal;

//port connection
//connect to port
assign d = result[32*0+:32];
assign e = result[32*1+:32];
assign f = result[32*2+:32];

//module instantiation
moduleA #(
.TIMEUNIT(16),
.WIDTH(WIDTH),
.ARRAYNUM(4)
)
nameA
(
.usrclk(deviceclk),  //i
.nreset(~devicerst), //o
.a(a),  //i
.b(b),  //i
.c(c),  //i
.alpha(alpha),  //o
.beta(beta),  //o
.gama(gama),  //o
.feedback(feedback),  //o
.start(start),  //o
.stop(stop),  //o
.pause(pause)  //o
);

moduleB #(
.WIDTH(WIDTH),
.ADDRESS(ADDRESS)
)
nameB
(
.deviceclk(deviceclk),
.devicerst(devicerst),
.port1(alpha),
.port2(beta),
.port3(gama),
.start(start),
.stop(stop),
.pause(pause)
);


//Automatically generate a memtype wire
//Descripted in normal wire
moduleC #(
.WIDTH(WIDTH)
)
nameCalpha
(
.clk(deviceclk),
.rst(devicerst),
.in(alpha),
.out(signal[32*0+:32])
);

moduleC #(
.WIDTH(WIDTH)
)
nameCbeta
(
.clk(deviceclk),
.rst(devicerst),
.in(beta),
.out(signal[32*1+:32])
);

moduleC #(
.WIDTH(WIDTH)
)
nameCgama
(
.clk(deviceclk),
.rst(devicerst),
.in(gama),
.out(signal[32*2+:32])
);

//generate duplicated instance all expanded
//the keyword duplicate is preserved
moduleD #(
.WIDTH(WIDTH)
)
nameD0
(
.clk(deviceclk),
.rst(devicerst),
.in(signal[32*0+:32]),
.out(result[32*0+:32])
);

moduleD #(
.WIDTH(WIDTH)
)
nameD1
(
.clk(deviceclk),
.rst(devicerst),
.in(signal[32*1+:32]),
.out(result[32*1+:32])
);

moduleD #(
.WIDTH(WIDTH)
)
nameD2
(
.clk(deviceclk),
.rst(devicerst),
.in(signal[32*2+:32]),
.out(result[32*2+:32])
);

//generate duplicated instance with gen grammar
generate
genvar i;

for (i = 0; i < 9; i = i + 1)
begin:nameEgenblk

moduleE #(
)
nameE
(
.clk(deviceclk),
.nrst(~devicerst),
.paravec(para[WIDTH*i+:WIDTH])
);

end
endgenerate

endmodule
```

<!-- The second way is to write a simple description file, and run -t with the option -c.

* -c :: (compliment) Compliment the simple description file ccv into a full-port ccv file. -->
<!-- 
A simple description file is shown as:
```Verilog
module topfilename //automatically generate accroding to filename

moduleA nameA

moduleB nameB

duplicate moduleC nameC 2 nogen

duplicate moduleD nameD 16 gen
``` -->

<!-- The converted ccv file is shown as:

```Verilog
module topfilename

moduleA nameA
    _ clk
    _ rst
    _ a
    _ b
    _ c
    _ alpha
    _ beta
    _ gama
    _ feedback
    _ start
    _ stop
    _ pause

moduleB nameB
    _ deviceclk
    _ devicerst
    _ port1
    _ port2
    _ port3
    _ start
    _ stop
    _ pause

moduleC nameC1
    _ clk
    _ rst
    _ in
    _ out

moduleC nameC2
    _ clk
    _ rst
    _ in
    _ out

moduleC nameC3
    _ clk
    _ rst
    _ in
    _ out

duplicate moduleD nameD 16 gen
``` -->

<!-- Without -c, the simple description file(.ccv) will directly convert into Verilog file(.v) according to -a -d. -->



A simple project is shown as:

```Verilog
|_::topmodule
    |nameA::moduleA
        |namea::cellA
        |nameb::cellB
            |namealpha::alpha
    |nameB::moduleB
    |nameC1::moduleC
    |nameC2::moduleC
```



###Circuit Check

* Port description & wire declaration 

    This tool is aiming at ports description and wire connection of top modules. It can check the potential declaration problems. If the problems fit the minimal fix rules, they are fixed, otherwise, warnings are gived.

* Mismatched port width check.

    For most time, small errors arise due to the implicit one bit declaration of undeclaration wires. Mismatched port width check throws warnings and add declarations. For other declared wires, warnings are provided.

* Multiple drive check.

    This is a quite common error. However, simulator accepts this but shows a uncertain state for the multiple drived signal. Other errors are provied until synthesis and DRC check. Multiple drive check are provied when update project.

###GUI
A designed GUI is shown as:
![gui](ccv.jpg)

##Implementation of Program

###Data Structure

#####FILE 

* JSON(.json)  
    The .ccv.project is a JSON file essentially, which requires a JSON file parsing and writing library.

* Verilog(.v)

* SystemVerilog(.sv)

* VHDL(.vhd)

* ConvenientCodingVerilog(.ccv)  
    Self-definined file format.

#####DATA

<!-- ```Haskell
data Module = Module {
    paralist    :: Paralist,
    modlist     :: Modlist,
    iolist      :: IOlist,
    wirelist    :: Wirelist
}
``` -->
###Command Parsing

Use `System.Console.GetOpt` to parse arguments.

To actually turn the list of command line flags getArgs gives us, into a useful list of abstract Flag values, we use the `getOpt` function, which returns a triple consisting of flags that were set, a list of any non-flag arguments, and a list of error messages.

Perform different operations according to different options:

```Haskell
data Flag
    = Email                 -- --email
    | Author                -- --author
    | Initial               -- --init
    | Update                -- --update
    | New                   -- -n
    | Top                   -- -t
    | Description           -- -d
    | Automatic             -- -a
    | Port                  -- -p
    | Compliments           -- -c
    | Version               -- -v
    | Hierarchy             -- -h
    | Formatize             -- -f
    | Merge                 -- -m
    | Split                 -- -s
    | Help                  -- --help
```

**Initial** :: Initialize a project.

* Create a .ccv.project in filepath. The .ccv.project file is a concealed JSON file.
There is a global configuration file in the ccv directory, which specifies the default project attributes.

* Scan project directory to analyze verilog files and build a directory tree and a project hierarchy.

A sample file (incomplete) is shown as:
```JSON
{
    "project": {
        "author": "authorname", 
        "contact": "emailaddress", 
        "version": "version", 
        "versionstep": "versionstep", 
        "directory": {
            "value": "project", 
            "node": [
                {
                    "value": "utilities", 
                    "node": [
                        {
                            "value": "parse.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "calc.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "conversion.v", 
                            "node": [ ]
                        }
                    ]
                }, 
                {
                    "value": "cells", 
                    "node": [
                        {
                            "value": "modulea.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "moduleb.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "modulec.v", 
                            "node": [ ]
                        }
                    ]
                }, 
                {
                    "value": "top.v", 
                    "node": [ ]
                }
            ]
        }, 
        "hierarchy": {
            "value": "", 
            "module": "top", 
            "file": "top.v", 
            "node": [
                {
                    "value": "modulea_i", 
                    "module": "modulea", 
                    "file": "cells/modulea.v", 
                    "node": [
                        {
                            "value": "log0", 
                            "module": "log", 
                            "file": "utilities/calc.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "log1", 
                            "module": "log", 
                            "file": "utilities/calc.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "sin0", 
                            "module": "sin", 
                            "file": "utilities/calc.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "power0", 
                            "module": "power", 
                            "file": "utilities/calc.v", 
                            "node": [ ]
                        }, 
                        {
                            "value": "parse_i", 
                            "module": "parse", 
                            "file": "utilities/parse.v", 
                            "node": [ ]
                        }
                    ]
                }, 
                {
                    "value": "moduleb_i", 
                    "module": "moduleb", 
                    "file": "cells/moduleb.v", 
                    "node": [ ]
                }
            ]
        }, 
        "modules": [
            {
                "name": "modulea", 
                "note": "/*First Stage Calculation module*/", 
                "file": "cells/modulea.v",
                "paras": [
                    {
                        "para": "WIDTH", 
                        "value": "64"
                    }, 
                    {
                        "para": "STRB", 
                        "value": "WIDTH / 8"
                    }
                ], 
                "ports": [
                    {
                        "port": "clk", 
                        "dir": "in", 
                        "type": "wire", 
                        "length": "1", 
                        "width": "1", 
                        "note": "//Clock"
                    }, 
                    {
                        "port": "rst", 
                        "dir": "in", 
                        "type": "wire", 
                        "length": "1", 
                        "width": "1", 
                        "note": "//Reset"
                    }, 
                    {
                        "port": "a", 
                        "dir": "in", 
                        "type": "wire", 
                        "length": "64", 
                        "width": "1", 
                        "note": "//Signal A inputs"
                    }, 
                    {
                        "port": "b", 
                        "dir": "out", 
                        "type": "wire", 
                        "length": "64", 
                        "width": "8", 
                        "note": "//Signal B outputs"
                    }
                ]
            }
        ]
    }
}

```

A global config file (.global.project) is shown as:

```JSON

{
"config":{
        "author": "authorname", 
        "contact": "emailaddress", 
        "version": "version", 
        "versionstep": "versionstep"
    }
}
```

The optional JSON parsers: 

```python
The RJson package :: http://hackage.haskell.org/package/RJson  
The json package  :: http://hackage.haskell.org/package/json
The aeson package :: https://hackage.haskell.org/package/aeson ** 
```

A directory related package:

```Python
The Directory-Tree Package :: http://hackage.haskell.org/package/directory-tree **
```

**Email** :: Set contact email.  
**Author** :: Set author name.

* Check the ccv directory first. If the configuration file doesn't exists, create a configuration file with the specified information. Otherwise, modify the configuration file with the specified information.

**Update** :: Update project.

* Check the project file first. The ccv will search from the current directory(or specified file path) and parental directories until it finds the project file or return `PROJECT_NOT_FOUND` error.

* Scan sub-directories recusively and analyze Verilog files. If any syntax errors on port descriptions and module instatiation are found, return `SYNTAX_ERROR` error and indicate the error code. 

* If two modules with the same name are found, return `NAME_DUPLICATE` error and indicate the duplicated modules.

* Update the project file with analyzed modules and ports.

**New** :: Create a verilog file.

* Check the ccv directory first and get the email and author.

* Check the ccv directory and get a COPYRIGHT DECLARATION file. If the declaration file does not exist, then create a template and a empty declaration file. The COPYRIGHT DECLARATION file is named as "copyright.txt", and the template file is named as "copyright_template.txt".

* Check current directory. If there is no file named as "filename.v", then create the new file. Otherwise return `FILE_DUPLICATION` options : `Cover the file(Y/N):`

The optional Verilog parsers are: 

```python
The Verilog Parser package :: https://github.com/tomahawkins/verilog  
The Netlist and Verilog Haskell Package :: https://github.com/pheaver/netlist-verilog    
```

**Top** :: Create top module.

* First check filename duplication : `Cover the file(Y/N):`

* Find project file directory and update.

* Parse .ccv file and build module graph (auto connection).

* Generate Verilog. 

``` Haskell
--Convesion Chain
ccvFile -> ccvGraph -> ccvDoc -> Verilog
ccvParse . ccvShow . ccvPrettify
```

*Options for* **Top**:

***Description*** :: Preserve annotations.

* Preserve the port annotations of the child modules.

***Automatic*** :: Connect the ports.

* Connect the same name ports. Otherwise, only connect user specified ports.

**Port** ::  Show port directions.

* Find project file and extract modules.

* Add port directions as annotaions.

**Compliment** :: Compliment ccv file.

* Find project file and extract modules.

* Expand ccv file into full port version.

*Options for* **Compliment**

***Guide*** :: Screen input mode.

* Find project file and extract hierarchy.

* Wait for the input of hierarchy expansion. 0 for only root, any number larger than the hierarchy amount will be bounded to the maximum hierarchy amount.

* Choose from: input, hierarchy, directory and modules.

* The input ends up with a EOF.

* Generate the expanded ccv file.

A typical compliment can be:
```python
output: Input(0), Project Hierarchy(1), Module Lists(2):

Input: 1

output: Show hierarchy level(0 for root only)

input: 1

output:
|_::moduleA
    |namea::cellA
    |nameb::cellB
|_::moduleB
|_::moduleC

output: Input(0), Project Hierarchy(1), Module Lists(2):

input: 2
moduleA
moduleB
moduleC
cellA
cellB
alpha

output: Input(0), Project Hierarchy(1), Module Lists(2):

input: 0

input: module topmodule
input: moduleA _
input: moduleB _
input: duplicate moduleC _ 3 nogen
input: duplicate moduleD _ 16 gen
input: EOF
```

The complimented ccv file is shown as:

```Verilog 

module topfilename

moduleA moduleA0
    _ clk
    _ rst
    _ a
    _ b
    _ c
    _ alpha
    _ beta
    _ gama
    _ feedback
    _ start
    _ stop
    _ pause

moduleB nameB0
    _ deviceclk
    _ devicerst
    _ port1
    _ port2
    _ port3
    _ start
    _ stop
    _ pause

moduleC nameC0
    _ clk
    _ rst
    _ in
    _ out

moduleC nameC1
    _ clk
    _ rst
    _ in
    _ out

moduleC nameC2
    _ clk
    _ rst
    _ in
    _ out

duplicate moduleD nameD 16 gen
```

**Version** :: Set version and notes.

* Check file revision history.

Serveral usage of -v is provided. -r represents for recursively.

```Python

template:
ccv -[vr] [verison | +versionstep | +] [file | directory] [descriptions]

//Apply to a file and set revision number.
ccv -v 1.0 moduleA.v "Formal version"

//Apply to a directory and set revision step
ccv -vr +0.1 src "Formal version"

??//Apply to multiple files
ccv -v + moduleA.v+moduleB.v "Cool"

```

**Hierarchy** :: Print project hierarchy.

* Find project file first.

* Print project hierarchy or file hierarchy.

```Verilog
template

ccv -H

output:
|_::topmodule
    |nameA::moduleA
        |namea::cellA
        |nameb::cellB
            |namealpha::alpha
    |nameB::moduleB
    |nameC1::moduleC
    |nameC2::moduleC

ccv -H src

output:
|src
    |utilities
        |calc.v
        |define.h
    |cells
        |moduleA.v
        |moduleB.v
|top.v
```

**Formatize** :: Formatize port declarations and coding style.

The usage is shown as:

```Python
ccv -f [95|01|05|vhd] [95|01] filename filepath

```

**Merge** :: Merge multiple files.

The usage is shown as:

```Verilog
//a.v
module moduleA(...);
...
endmodule

//b.v
module moduleB(...);
...
endmodule


ccv -m add.v a.v b.v
//add.v
module moduleA(...);
...
endmodule

module moduleB(...);
...
endmodule
```

**Split** :: Split files.

The split process is the opposite of merge.

```Verilog
ccv -s add.v

add.v
moduleA.v
moduleB.v
```

###Funtions
####Initial
* Check the ccv directory first. If no global config file is found, then create one.

```Haskell
checkConfigFile :: Bool

checkFile :: String -> String -> Bool
checkFile fileName filePath

createConfig :: ()

buildConfigData :: ConfigData

convertConfigData :: ConfigData -> JDoc

writeJSON :: JDoc -> IO()

```

*  Check the specified directory. If project file exists, throw a warning and wait user's choice.

```Haskell
checkProjectFile :: Bool

checkFile

throwWarning

getUserChoice
```

* Scan the current directory and sub-directory recursively(-r) to build a file tree.

```Haskell
buildFileTree :: String -> FileTree
buildFileTree filepath

getCurrentDir :: String

getSubDir :: String -> String

```

* Parse the file with Verilog file extensions to build a project hierarchy.

```Haskell
buildProjectTree :: FileTree -> ProjectTree

parseVerilogFile :: VerilogFile -> VerilogData
parseVerilogFile verilogFile

getVerilogList :: FileTree -> VerilogList
getVerilogList :: fileTree

readVerilogFile :: String -> String -> VerilogFile
readVerilogFile filename filepath
```

* Create project file.

```Haskell
buildProjectFile :: ConfigData -> FileTree -> ProjectTree -> ModuleList -> ProjectFile

ConvertProjectFile :: ProjectFile -> JDoc


```

####Email Author

* Check the ccv directory first. If no global config file is found, then create one with specified information.


* Read config file, replace information and write out.

####Update 

* Find project file from current directory up to parental directories. If no project file is found, throw `PROJECT_NOT_FOUND` error.

```Haskell
getCurrentDir :: String

getParentalDic :: String -> String

checkProjectFile

```

* Parse project file to get configuration.

```Haskell
parseProjectFile :: String -> ProjectFile

getConfigData :: ProjectFile -> ConfigData
getFileTree :: ProjectFile -> FileTree
getProjectTree :: ProjectFile -> ProjectTree
getModuleList :: ProjectFile -> ModuleList

```

* Scan the current directory and sub-directory recursively(-r) to build a file tree.

* Parse the file with Verilog file extensions to build a project hierarchy.

* Create project file.

####New

* Check the current directory. If a file with the same name exits, then throw a warning and wait for the user's choice.

* Check project file first. If no project file is found, check global config file. Same as **initial**.

* Parse one of those files to get the configuration.

* Build Verilog file with the configuration.

```Haskell
buildVerilogFile :: ConfigData -> VerilogFile

prettifyVerilog :: VerilogFile -> VerilogString

```

* Write Verilog.


####Top

* Check Project File.

* Parse ccv file.

* Convert description of ccv into Verilog AST. Throw warning if the module name mismatches.

* Add comments and automatically wire connection.

* Write Verilog.



###Future Extensions
**Library**

* import :: import hardware library

```Haskell 
import Detect as Detect

```

* library :: library specification

```Haskell
library Detect 
```

