with Ada.Text_IO; use Ada.Text_IO;

with Addresses;    use Addresses;
with Base64;       use Base64;
with Cells;        use Cells;
with Cryptography; use Cryptography;
with Mnemonics;    use Mnemonics;
with Contracts;    use Contracts;
with Types;        use Types;

procedure Vaniton is

begin
   while True loop
      declare
         Phrase          : Mnemonic := Generate;
         Keys            : Key_Pair := To_Key_Pair (Phrase);
         Wallet_Contract : WalletV3R2_Contract;
      begin
         Create (Wallet_Contract, Keys.Public_Key, 0);
         Put_Line
           (To_String (Get_Address (Wallet_Contract)) & "|" &
            To_String (Phrase));
      end;
   end loop;
end Vaniton;
