with Interfaces; use Interfaces;

with Addresses; use Addresses;
with Cells;     use Cells;
with Types;     use Types;

package Contracts is
   type Wallet_Contract is
     (Simple_R1, Simple_R2, Simple_R3, V2_R1, V2_R2, V3_R1, V3_R2, V4_R1,
      V4_R2);

   type Contract is record
      Kind      : Wallet_Contract;
      Code      : aliased Cell;
      Data      : aliased Cell;
      Workchain : Integer_8 := 0;
   end record;

   function Create
     (Public_Key : Byte_Array; Workchain : Integer_8 := 0;
      Kind       : Wallet_Contract := V3_R2) return Contract;
   function Get_Address (This_Original : in Contract) return Address;
end Contracts;
