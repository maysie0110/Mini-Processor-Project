library verilog;
use verilog.vl_types.all;
entity main is
    port(
        upd             : in     vl_logic;
        exe             : in     vl_logic;
        instruction     : in     vl_logic_vector(15 downto 0);
        out1            : out    vl_logic_vector(6 downto 0);
        out2            : out    vl_logic_vector(6 downto 0)
    );
end main;
