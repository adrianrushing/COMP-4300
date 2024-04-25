-- lab4_control_v3.vhd

use work.bv_arithmetic.all;
use work.dlx_types.all;

-- This entity chops up a 32-bit word into the relevant component parts.
-- If a particular output is not used for a particular instruction type
-- that field is set to zero. The input from the decoder is the instruction
-- register. It operates in a purely combinational-logic mode. The controller
-- makes use of its outputs when appropriate, ignores them otherwise.
-- For R-type ALU instruction format in Figure 2.27, 
-- reg0p1 is labelled "rs" in Figure 2.27, regOp2 is labelled "rt", and
-- regDest is labelled "rd".
-- For I-type ALU instruction format in Figure 2.27
-- regOp1 is "rs" and regDest is "rt"

entity mips_decoder is
  
  port (
    instruction : in dlx_word;
    regOp1,regOp2,regDest: out register_index;
    alu_func: out func_code; 
    immediate: out half_word;
    opcode: out opcode_type   
  ); 
end mips_decoder;

architecture behaviour of mips_decoder is
begin
   process(instruction)
   begin
	opcode <= instruction(31 downto 26) after 5 ns;
	-- Extract register operands and destination register
	regOp1 <= instruction(25 downto 21) after 5 ns;
        regOp2 <= instruction(20 downto 16) after 5 ns;
        regDest <= instruction(15 downto 11) after 5 ns;
        -- Extract ALU function code
        alu_func <= instruction (5 downto 0) after 5 ns; -- Line 39
        -- Extract immediate value
        immediate <= instruction(15 downto 0) after 5 ns;
   end process;
end behaviour;

use work.bv_arithmetic.all;
use work.dlx_types.all;

-- This entity controls the DLX processor. It is driven by the external
-- clock signal, and takes inputs from the decoder also. It drives the
-- input of every latch on the chip, and the control input to every
-- mux, as well as sending function codes
-- to the ALU and processing ALU error codes

entity mips_controller is
 
  port (
    opcode: in  opcode_type;
    alu_func: in func_code;
    clock: in bit; 
    aluA_mux: out bit;
    aluB_mux: out bit;
    alu_oper: out alu_operation_code;
    alu_signed: out bit; 
    write_mux: out bit;
    ir_clock: out bit;
    IM_clock: out bit; 
    pc_clock: out bit;
    npc_clock: out bit;
    imm_clock: out bit;
    alu_out_clock: out bit; 
    lmd_clock: out bit; 
    regA_clock,regB_clock: out bit;
    DM_clock: out bit;
    DM_readnotwrite: out bit;
    reg_clock: out bit;
    reg_readnotwrite: out bit;
    regA_index_mux: out bit; 
    equal_out: in bit;
    cond_out: out bit 
    );
    
end mips_controller;

architecture behaviour of mips_controller is

begin  -- behaviour

  behav: process(opcode,alu_func,clock) is
     -- cuurent state of the machine 
     type state_type is range 1 to 5;                                
     variable state: state_type := 1;                               
  begin                                
     if clock'event and clock = '1' then
       case state is 
         when 1 =>

	    -- Cycle 1: Instruction fetch (IF). Applies to all instructions
	    -- IR <- InstructionMemory[PC]
	    -- NPC <- PC + 4
	    -- set clock to 1 so the npc is good to go
	    npc_clock <= '1' after 5 ns;
	    -- grabs the instruction
	    IM_clock <= '1' after 5 ns;
	    -- put the instruction in the register
	    ir_clock <= '1' after 5 ns;

	    -- Reset Cycle 2
	    imm_clock <= '0' after 5 ns;
	    reg_readnotwrite <= '0' after 5 ns;
	    regA_clock <= '0' after 5 ns;
	    regB_clock <= '0' after 5 ns;
	    reg_clock <= '0' after 5 ns;

            -- Reset Cycle 3
	    alu_out_clock <= '0' after 5 ns;
	    reg_readnotwrite <= '0' after 5 ns;

	    -- Reset Cycle 4
	    DM_clock <= '0' after 5 ns;
	    lmd_clock <= '0' after 5 ns;
	    pc_clock <= '0' after 5 ns;
	    regA_index_mux <= '1' after 5 ns;
	    reg_readnotwrite <= '0' after 5 ns;
	    DM_readnotwrite <= '1' after 5 ns;

	    state := 2;      
         when 2 =>
	    -- Cycle 2:  Instruction decode (ID)/register fetch for all instructions. Note the values in A,B,Imm
	    -- may not all be used or even make sense for some instructions. In those cases we just won?t use
	    -- those values. It does no harm to store them in A,B,Imm. 
	    -- A   <- Registers[rs]
	    -- B   <- Registers[rt]
	    -- Imm <- sext(immediate field of IR)
	    imm_clock <= '1' after 5 ns;
	    -- set up and read the source operands from the regfile
	    reg_readnotwrite <= '1' after 5 ns;

	    regA_clock <= '1' after 5 ns;
	    regB_clock <= '1' after 5 ns;
	    -- this is tricky, you have to delay reg_clock for long enough
	    -- that you don't accidentally write something into the wrong
	    -- reg from the writeback cycle
	    reg_clock <= '1' after 5 ns;
            state := 3; 
         when 3 =>

	    -- Cycle 3: Execute (EX) ALU op/ compute effective address (load/store/jump)
	    
	    -- For memory reference operations
	    -- ALU Output <- A + Imm
	    
	    -- For register-register operations
	    -- ALU Output <- A func B
	    -- Where func is specified by the opcode and function code
	    
	    -- For register-immediate ALU operations
	    -- ALU <- A op Imm
	    
	    alu_out_clock <= '1' after 5 ns;
	    reg_readnotwrite <= '1' after 5 ns;

	    case opcode is
		when "000000" =>
		   case alu_func is
			-- REGISTER - REGISTER OPERATIONS
			when "100000" => -- ADD 
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "0000" after 5 ns;
			      alu_signed <= '1' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "100001" => -- ADDU
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "0000" after 5 ns;
			      alu_signed <= '0' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "100010" => -- SUB
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "0001" after 5 ns;
			      alu_signed <= '1' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "100011" => -- SUBU
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "0001" after 5 ns;
			      alu_signed <= '0' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "001110" => -- MULT
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "1110" after 5 ns;
			      alu_signed <= '1' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "010110" => -- MULTU
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "1110" after 5 ns;
			      alu_signed <= '0' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "100100" => -- AND
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "0010" after 5 ns;
			      alu_signed <= '1' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "100101" => -- OR
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "0011" after 5 ns;
			      alu_signed <= '1' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "101010" => -- SLT
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "1101" after 5 ns;
			      alu_signed <= '1' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when "101011" => -- SLTU
			      DM_readnotwrite <= '1' after 5 ns;
			      aluA_mux <= '0' after 5 ns;
			      aluB_mux <= '1' after 5 ns;
			      alu_oper <= "1101" after 5 ns;
			      alu_signed <= '0' after 5 ns;
			      write_mux <= '0' after 5 ns;
			      cond_out <= '1' after 5 ns;
			when others => null;
		   end case;
			-- REGISTER -- IMMEDIATE
		when "001000" => -- ADDI
		    DM_readnotwrite <= '1' after 5 ns;
		    aluA_mux <= '0' after 5 ns;
		    aluB_mux <= '0' after 5 ns;
		    alu_oper <= "0000" after 5 ns;
		    alu_signed <= '0' after 5 ns;
		    write_mux <= '0' after 5 ns;
		    cond_out <= '1' after 5 ns;	
		when "001001" => -- ADDIU
		    DM_readnotwrite <= '1' after 5 ns;
		    aluA_mux <= '0' after 5 ns;
		    aluB_mux <= '0' after 5 ns;
		    alu_oper <= "0000" after 5 ns;
		    alu_signed <= '0' after 5 ns;
		    write_mux <= '0' after 5 ns;
		    cond_out <= '1' after 5 ns;	
		when "001100" => -- ANDI
		    DM_readnotwrite <= '1' after 5 ns;
		    aluA_mux <= '0' after 5 ns;
		    aluB_mux <= '0' after 5 ns;
		    alu_oper <= "0010" after 5 ns;
		    alu_signed <= '0' after 5 ns;
		    write_mux <= '0' after 5 ns;
		    cond_out <= '1' after 5 ns;	
		when "100011" => -- LW
		    DM_readnotwrite <= '1' after 5 ns;
		    aluA_mux <= '0' after 5 ns;
		    aluB_mux <= '0' after 5 ns;
		    alu_signed <= '0' after 5 ns;
		    write_mux <= '1' after 5 ns;
		    cond_out <= '1' after 5 ns;	
		when "101011" => -- SW
		    DM_readnotwrite <= '0' after 5 ns;		
		    aluA_mux <= '0' after 5 ns;
		    aluB_mux <= '0' after 5 ns;
		    alu_signed <= '0' after 5 ns;
		    write_mux <= '1' after 5 ns;
		    cond_out <= '1' after 5 ns;	
	    	when others => null;
	     end case;
	-- BONUS
	
	    -- Where op is specified by the opcode. 
	    -- For conditional branch instruction BEQ (extra credit) you will need to implement an ALU
	    -- function that multiplies the lower input by 4 before adding to the upper input, to compute the
	    -- jump target address. 
	    -- ALU output <- NPX + (Imm <<)
	    -- Zero <- (A == B)
	    -- For unconditional branch instructions the immediate is bits 0-25, and it is not multiplied by 4
	    -- before using it as the jump target address. Implementing this instruction (extra credit only)
	    -- requires modification of the datapath and interconnect files.
            state := 4; 
         when 4 =>
	    -- Cycle 4: Memory access (MEM)/branch completion 
	    -- For load instruction
	    -- LMD <- DataMemory[ALU output]
	    -- For store instruction 
	    -- DataMemory[ALU output] <- B
	    -- For all other instructions nothing happens in this cycle
            DM_clock <= '1' after 5 ns;
	    lmd_clock <= '1' after 5 ns;
	    pc_clock <= '1' after 5 ns;

	    if opcode = "100011" then -- LW
		DM_readnotwrite <= '1' after 5 ns;
	    elsif opcode = "101011" then -- SW
		DM_readnotwrite <= '0' after 5 ns;
	    else 
	    end if;
	    state := 5; 
         when 5 =>
	    -- Cycle 5: Write back (WB):
	    -- For ALU operations (reg-reg or reg-imm):
	    -- Registers[rd] <- ALU output
	    -- For Load
	    -- Regs <- LMD
	    -- For all instructions except taken branch
	    -- PC <- NPC
	    -- For taken branch
	    -- PC <- ALU output
	    reg_clock <= '1' after 5 ns;

	    if( opcode = "000000" or opcode = "001000" or opcode = "001001" or opcode = "001010" or opcode = "001100" or opcode = "100011") then
              regA_index_mux <= '1' after 5 ns; 
              reg_readnotwrite <= '0' after 5 ns;
            else
              regA_index_mux <= '0' after 5 ns;
              reg_readnotwrite <= '1' after 5 ns; 
            end if;
            DM_readnotwrite <= '1' after 5 ns; 
	
            state := 1; 
         when others => null;
       end case;
     else
       if clock'event and clock = '0' then 
		
       end if;
       
     end if;
  end process behav;                                 

end behaviour;