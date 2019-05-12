library verilog;
use verilog.vl_types.all;
entity main_vlg_check_tst is
    port(
        out1            : in     vl_logic_vector(6 downto 0);
        out2            : in     vl_logic_vector(6 downto 0);
        sampler_rx      : in     vl_logic
    );
end main_vlg_check_tst;
