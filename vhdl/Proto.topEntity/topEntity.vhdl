-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.topEntity_types.all;
library work;

entity topEntity is
  port(-- clock
       clk    : in topEntity_types.clk_System;
       x      : in std_logic_vector(17 downto 0);
       y      : in std_logic_vector(17 downto 0);
       result : out std_logic_vector(17 downto 0));
end;

architecture structural of topEntity is
  signal x_0      : topEntity_types.FoFloat;
  signal y_0      : topEntity_types.FoFloat;
  signal result_0 : topEntity_types.FoFloat;

begin
  x_0 <= topEntity_types.FoFloat'(topEntity_types.fromSLV(x));

  y_0 <= topEntity_types.FoFloat'(topEntity_types.fromSLV(y));

  plusFloat_inst_block : block
    component plusFloat port
      ( clk : in std_logic
      ; X : in topEntity_types.FoFloat
      ; Y : in topEntity_types.FoFloat
      ; R : out topEntity_types.FoFloat );
    end component;
  begin
    plusFloat_inst : plusFloat
      port map
        ( clk => clk
        , X   => x_0
        , Y   => y_0
        , R   => result_0 );


  end block;

  result <= topEntity_types.toSLV(topEntity_types.FoFloat'(result_0));


end;

