-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Proto_testBench_types.all;

entity testBench is
  port(result : out std_logic_vector(17 downto 0));
end;

architecture structural of testBench is
  signal packedFields                   : std_logic_vector(17 downto 0);
  signal g1                             : std_logic_vector(1 downto 0);
  signal g2                             : std_logic;
  signal g3                             : std_logic_vector(3 downto 0);
  signal g4                             : std_logic_vector(10 downto 0);
  signal \c$app_arg\                    : Proto_testBench_types.FoFloat;
  signal \c$ds_app_arg\                 : Proto_testBench_types.index_5;
  signal \c$ds_app_arg_0\               : Proto_testBench_types.FoFloat;
  signal s                              : Proto_testBench_types.index_5 := to_unsigned(0,3);
  signal \c$ds_app_arg_1\               : Proto_testBench_types.index_5;
  signal \c$ds_app_arg_2\               : Proto_testBench_types.FoFloat;
  signal s_0                            : Proto_testBench_types.index_5 := to_unsigned(0,3);
  signal \c$app_arg_0\                  : Proto_testBench_types.rst_System;
  signal \c$app_arg_1\                  : Proto_testBench_types.clk_System;
  signal clk_0                          : Proto_testBench_types.clk_System;
  signal x_0                            : Proto_testBench_types.FoFloat;
  signal x_1                            : std_logic_vector(17 downto 0);
  signal y_0                            : Proto_testBench_types.FoFloat;
  signal y_1                            : std_logic_vector(17 downto 0);
  signal result_1                       : std_logic_vector(17 downto 0);
  signal result_2                       : Proto_testBench_types.FoFloat;
  signal \c$ds_app_arg_selection_res\   : boolean;
  signal \c$vec\                        : Proto_testBench_types.array_of_FoFloat(0 to 4);
  signal \c$ds_app_arg_selection_res_0\ : boolean;
  signal \c$vec_0\                      : Proto_testBench_types.array_of_FoFloat(0 to 4);

begin
  packedFields <= (std_logic_vector'(std_logic_vector'(((std_logic_vector'(std_logic_vector'((g1)) & std_logic_vector'(((std_logic_vector'(0 => g2)))))))) & std_logic_vector'(((std_logic_vector'(std_logic_vector'((g3)) & std_logic_vector'(((std_logic_vector'((g4)))))))))));

  g1 <= \c$app_arg\.FoFloat_sel0_ext;

  g2 <= \c$app_arg\.FoFloat_sel1_r_sign;

  g3 <= \c$app_arg\.FoFloat_sel2_exponentVal;

  g4 <= \c$app_arg\.FoFloat_sel3_fractionalVal;

  result <= (std_logic_vector'(packedFields));

  clk_0 <= \c$app_arg_1\;

  x_0 <= \c$ds_app_arg_2\;

  y_0 <= \c$ds_app_arg_0\;

  x_1 <= Proto_testBench_types.toSLV(Proto_testBench_types.FoFloat'(x_0));

  y_1 <= Proto_testBench_types.toSLV(Proto_testBench_types.FoFloat'(y_0));

  topEntity_capp_arg : entity topEntity.topEntity
    port map
      (clk_0, x_1, y_1, result_1);

  result_2 <= Proto_testBench_types.FoFloat'(Proto_testBench_types.fromSLV(result_1));

  \c$app_arg\ <= result_2;

  \c$ds_app_arg_selection_res\ <= s < to_unsigned(4,3);

  \c$ds_app_arg\ <= s + to_unsigned(1,3) when \c$ds_app_arg_selection_res\ else
                    s;

  \c$vec\ <= Proto_testBench_types.array_of_FoFloat'( ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                    , FoFloat_sel1_r_sign => '0'
                                                    , FoFloat_sel2_exponentVal => std_logic_vector'(x"7")
                                                    , FoFloat_sel3_fractionalVal => std_logic_vector'("00110011010") )
                                                    , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                    , FoFloat_sel1_r_sign => '0'
                                                    , FoFloat_sel2_exponentVal => std_logic_vector'(x"7")
                                                    , FoFloat_sel3_fractionalVal => std_logic_vector'("01100110100") )
                                                    , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                    , FoFloat_sel1_r_sign => '0'
                                                    , FoFloat_sel2_exponentVal => std_logic_vector'(x"8")
                                                    , FoFloat_sel3_fractionalVal => std_logic_vector'("00100110100") )
                                                    , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                    , FoFloat_sel1_r_sign => '1'
                                                    , FoFloat_sel2_exponentVal => std_logic_vector'(x"9")
                                                    , FoFloat_sel3_fractionalVal => std_logic_vector'("00010011010") )
                                                    , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                    , FoFloat_sel1_r_sign => '1'
                                                    , FoFloat_sel2_exponentVal => std_logic_vector'(x"9")
                                                    , FoFloat_sel3_fractionalVal => std_logic_vector'("01100110100") ) );

  -- index begin
  indexVec : block
    signal vec_index : integer range 0 to 5-1;
  begin
    vec_index <= to_integer((signed(std_logic_vector(resize(s,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$ds_app_arg_0\ <= \c$vec\(vec_index);
  end block;
  -- index end

  -- register begin
  s_register : process(\c$app_arg_1\,\c$app_arg_0\)
  begin
    if \c$app_arg_0\ =  '1'  then
      s <= to_unsigned(0,3);
    elsif rising_edge(\c$app_arg_1\) then
      s <= \c$ds_app_arg\;
    end if;
  end process;
  -- register end

  \c$ds_app_arg_selection_res_0\ <= s_0 < to_unsigned(4,3);

  \c$ds_app_arg_1\ <= s_0 + to_unsigned(1,3) when \c$ds_app_arg_selection_res_0\ else
                      s_0;

  \c$vec_0\ <= Proto_testBench_types.array_of_FoFloat'( ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                      , FoFloat_sel1_r_sign => '0'
                                                      , FoFloat_sel2_exponentVal => std_logic_vector'(x"7")
                                                      , FoFloat_sel3_fractionalVal => std_logic_vector'("00110011010") )
                                                      , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                      , FoFloat_sel1_r_sign => '0'
                                                      , FoFloat_sel2_exponentVal => std_logic_vector'(x"A")
                                                      , FoFloat_sel3_fractionalVal => std_logic_vector'("11000000000") )
                                                      , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                      , FoFloat_sel1_r_sign => '0'
                                                      , FoFloat_sel2_exponentVal => std_logic_vector'(x"B")
                                                      , FoFloat_sel3_fractionalVal => std_logic_vector'("01110000000") )
                                                      , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                      , FoFloat_sel1_r_sign => '1'
                                                      , FoFloat_sel2_exponentVal => std_logic_vector'(x"9")
                                                      , FoFloat_sel3_fractionalVal => std_logic_vector'("00010011010") )
                                                      , ( FoFloat_sel0_ext => std_logic_vector'("01")
                                                      , FoFloat_sel1_r_sign => '1'
                                                      , FoFloat_sel2_exponentVal => std_logic_vector'(x"9")
                                                      , FoFloat_sel3_fractionalVal => std_logic_vector'("01100110100") ) );

  -- index begin
  indexVec_0 : block
    signal vec_index_0 : integer range 0 to 5-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(s_0,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$ds_app_arg_2\ <= \c$vec_0\(vec_index_0);
  end block;
  -- index end

  -- register begin
  s_0_register : process(\c$app_arg_1\,\c$app_arg_0\)
  begin
    if \c$app_arg_0\ =  '1'  then
      s_0 <= to_unsigned(0,3);
    elsif rising_edge(\c$app_arg_1\) then
      s_0 <= \c$ds_app_arg_1\;
    end if;
  end process;
  -- register end

  -- resetGen begin
  resetGen : block
    constant reset_delay : time := 100000 ps - 1 ps + (integer'(1) * 10000 ps);
  begin
  -- pragma translate_off
  \c$app_arg_0\
    <= '1',
       '0' after reset_delay;
  -- pragma translate_on
  end block;
  -- resetGen end

  -- tbClockGen begin
  -- pragma translate_off
  clkGen : process is
    constant half_periodH : time := 10000000 fs / 2;
    constant half_periodL : time := 10000000 fs - half_periodH;
  begin
    \c$app_arg_1\ <= '0';
    wait for 100000 ps;
    loop
      \c$app_arg_1\ <= not \c$app_arg_1\;
      wait for half_periodH;
      \c$app_arg_1\ <= not \c$app_arg_1\;
      wait for half_periodL;
    end loop;
    wait;
  end process;
  -- pragma translate_on
  -- tbClockGen end


end;

