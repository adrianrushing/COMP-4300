USE work.dlx_types.all;
USE work.bv_arithmetic.all;

entity mips_bit_register is 
	generic(prop_delay: time := 5 ns);
	port(in_val: in bit; 
 	     clock: in bit;
	     out_val: out bit); 
end entity mips_bit_register; 

architecture behaviour of mips_bit_register is 
begin
	mips_bit_register: process(in_val, clock)
	begin
		if(clock = '1') then
			out_val <= in_val after prop_delay;
		end if;
	end process;
end behaviour;