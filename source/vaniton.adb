with Ada.Text_IO; use Ada.Text_IO;

with Addresses;    use Addresses;
with Base64;       use Base64;
with Cells;        use Cells;
with Cryptography; use Cryptography;
with Mnemonics;    use Mnemonics;
with Contracts;    use Contracts;
with Types;        use Types;

procedure Vaniton is
   My_Mnemonic_Phrase : String :=
     "describe mimic shop crowd fat wire toe grid street submit grow play history depth modify bomb jeans icon enforce horse super sting tilt radio";

   My_Mnemonic : Mnemonic := From_String (My_Mnemonic_Phrase);
   My_Key_Pair : Key_Pair := To_Key_Pair (My_Mnemonic);
begin
   Put_Line
     ("Simple_R1: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_Simple_R1))));
   Put_Line
     ("Simple_R2: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_Simple_R2))));
   Put_Line
     ("Simple_R3: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_Simple_R3))));
   Put_Line
     ("V2_R1: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_V2_R1))));
   Put_Line
     ("V2_R2: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_V2_R2))));
   Put_Line
     ("V3_R1: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_V3_R1))));
   Put_Line
     ("V3_R2: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_V3_R2))));
   Put_Line
     ("V4_R1: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_V4_R1))));
   Put_Line
     ("V4_R2: " &
      To_String
        (Get_Address
           (Create
              (Public_Key => My_Key_Pair.Public_Key,
               Kind       => Wallet_Contract_V4_R2))));
end Vaniton;
