library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package Proto_testBench_types is

  subtype index_5 is unsigned(2 downto 0);
  subtype clk_System is std_logic;
  subtype rst_System is std_logic;


  type FoFloat is record
    FoFloat_sel0_ext : std_logic_vector(1 downto 0);
    FoFloat_sel1_r_sign : std_logic;
    FoFloat_sel2_exponentVal : std_logic_vector(3 downto 0);
    FoFloat_sel3_fractionalVal : std_logic_vector(10 downto 0);
  end record;
  type array_of_FoFloat is array (integer range <>) of FoFloat;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (p : FoFloat) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return FoFloat;
  function toSLV (value :  array_of_FoFloat) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_FoFloat;
end;

package body Proto_testBench_types is
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function fromSLV (slv : in std_logic_vector) return unsigned is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return unsigned(islv);
  end;
  function toSLV (sl : in std_logic) return std_logic_vector is
  begin
    return std_logic_vector'(0 => sl);
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic is
    alias islv : std_logic_vector (0 to slv'length - 1) is slv;
  begin
    return islv(0);
  end;
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (p : FoFloat) return std_logic_vector is
  begin
    return (toSLV(p.FoFloat_sel0_ext) & toSLV(p.FoFloat_sel1_r_sign) & toSLV(p.FoFloat_sel2_exponentVal) & toSLV(p.FoFloat_sel3_fractionalVal));
  end;
  function fromSLV (slv : in std_logic_vector) return FoFloat is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 1)),fromSLV(islv(2 to 2)),fromSLV(islv(3 to 6)),fromSLV(islv(7 to 17)));
  end;
  function toSLV (value :  array_of_FoFloat) return std_logic_vector is
    alias ivalue    : array_of_FoFloat(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 18);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 18) + 1 to i*18) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_FoFloat is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_FoFloat(0 to slv'length / 18 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 18 to (i+1) * 18 - 1));
    end loop;
    return result;
  end;
end;

