library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package topEntity_types is

  subtype clk_System is std_logic;

  type FoFloat is record
    FoFloat_sel0_ext : std_logic_vector(1 downto 0);
    FoFloat_sel1_r_sign : std_logic;
    FoFloat_sel2_exponentVal : std_logic_vector(3 downto 0);
    FoFloat_sel3_fractionalVal : std_logic_vector(10 downto 0);
  end record;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (p : FoFloat) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return FoFloat;
end;

package body topEntity_types is
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
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
  function toSLV (p : FoFloat) return std_logic_vector is
  begin
    return (toSLV(p.FoFloat_sel0_ext) & toSLV(p.FoFloat_sel1_r_sign) & toSLV(p.FoFloat_sel2_exponentVal) & toSLV(p.FoFloat_sel3_fractionalVal));
  end;
  function fromSLV (slv : in std_logic_vector) return FoFloat is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 1)),fromSLV(islv(2 to 2)),fromSLV(islv(3 to 6)),fromSLV(islv(7 to 17)));
  end;
end;

