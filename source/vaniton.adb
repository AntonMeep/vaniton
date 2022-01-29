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

   V3R2 : WalletV3R2_Contract;
begin
   Put_Line ("Original mnemonic: " & My_Mnemonic_Phrase);
   Put_Line ("Loaded mnemonic  : " & To_String (My_Mnemonic));
   Put_Line ("");
   Put_Line
     ("Original public key  : 97ce851962898e52562ea91f8d98574371c2d8035e04d0d6b0a98a628ae19409");
   Put_Line
     ("Calculated public key: " & To_Hex_String (My_Key_Pair.Public_Key));
   Put_Line ("");
   Put_Line
     ("Original secret key  : c16e2fa8d39deaf2d5f0f86f49c700444bfe2a70f2cfdb7edcf4bcc6bd469ce597ce851962898e52562ea91f8d98574371c2d8035e04d0d6b0a98a628ae19409");
   Put_Line
     ("Calculated secret key: " & To_Hex_String (My_Key_Pair.Secret_Key));
   Put_Line ("");
   Put_Line
     ("Original private key base64  : wW4vqNOd6vLV8PhvSccAREv+KnDyz9t+3PS8xr1GnOU=");
   Put_Line
     ("Calculated private key base64: " &
      To_Base64 (My_Key_Pair.Secret_Key (1 .. 32)));
   Put_Line ("");

   Create (V3R2, My_Key_Pair.Public_Key, 0);
   Put_Line
     ("Original V3R2 smart contract code bits : FF0020DD2082014C97BA218201339CBAB19F71B0ED44D0D31FD31F31D70BFFE304E0A4F2608308D71820D31FD31FD31FF82313BBF263ED44D0D31FD31FD3FFD15132BAF2A15144BAF2A204F901541055F910F2A3F8009320D74A96D307D402FB00E8D101A4C8CB1FCB1FCBFFC9ED54");
   Put_Line
     ("Generated V3R2 smart contract code bits: " &
      To_Hex_String (Bits (V3R2.Code)));

   Put_Line ("");
   Put_Line
     ("Original account address  : EQDIpYGcfw8r36TYPhfa3kyErKDFrFFHAvjizYekZsNsuBVb");
   Put_Line ("Calculated account address: " & To_String (Get_Address (V3R2)));
end Vaniton;
