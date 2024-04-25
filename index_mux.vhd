USE work.dlx_types.all;
USE work.bv_arithmetic.all;

entity index_mux is
     port (input_1,input_0 : in register_index; 
	   which: in bit; 
	   output: out register_index);
end entity index_mux;


architecture behaviour of index_mux is
begin
    mux_process: process(input_1, input_0, which)
    begin
        if which = '0' then
            output <= input_0;
        else
            output <= input_1;
        end if;
    end process mux_process;
end behaviour;