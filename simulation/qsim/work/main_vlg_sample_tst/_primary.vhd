library verilog;
use verilog.vl_types.all;
entity main_vlg_sample_tst is
    port(
        exe             : in     vl_logic;
        instruction     : in     vl_logic_vector(15 downto 0);
        upd             : in     vl_logic;
        sampler_tx      : out    vl_logic
    );
end main_vlg_sample_tst;
