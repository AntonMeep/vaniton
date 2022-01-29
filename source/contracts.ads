with Interfaces; use Interfaces;

with Addresses; use Addresses;
with Cells;     use Cells;
with Types;     use Types;

package Contracts is
   type Contract is abstract tagged record
      Code      : aliased Cell;
      Data      : aliased Cell;
      Workchain : Integer_8;
   end record;

   procedure Create
     (This      : in out Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0) is abstract;
   function Get_Address (This : in out Contract) return Address;

   type WalletV3R1_Contract is new Contract with null record;
   type WalletV3R2_Contract is new Contract with null record;
   type WalletV4_Contract is new Contract with null record;

   procedure Create
     (This      : in out WalletV3R1_Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0);
   procedure Create
     (This      : in out WalletV3R2_Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0);
   procedure Create
     (This      : in out WalletV4_Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0);
end Contracts;
