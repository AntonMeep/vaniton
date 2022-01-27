with Ada.Text_IO; use Ada.Text_IO;

with Addresses;    use Addresses;
with Cryptography; use Cryptography;
with Mnemonics;    use Mnemonics;
with Contracts;    use Contracts;
with Types;        use Types;

procedure Vaniton is
   V3R1 : WalletV3R1_Contract;
   Mnm  : Mnemonic := Generate;
   KP   : Key_Pair := To_Key_Pair (Mnm);
begin
   Create (V3R1, KP.Public_Key);
   Put_Line (To_String (Get_Address (V3R1)));
end Vaniton;
