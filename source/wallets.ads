with Interfaces; use Interfaces;

with Addresses; use Addresses;
with Types;     use Types;

package Wallets is
   type Wallet_Kind is
     (Simple_R1, Simple_R2, Simple_R3, V2_R1, V2_R2, V3_R1, V3_R2, V4_R1,
      V4_R2);

   function Get_Wallet_Address
     (Public_Key : Byte_Array; Workchain : Integer_8 := 0;
      Kind       : Wallet_Kind := V3_R2) return Address;
end Wallets;
