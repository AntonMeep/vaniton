with Ada.Text_IO; use Ada.Text_IO;

with Cryptography;
with Mnemonics;
with Types; use Types;

procedure Vaniton is

begin
   Put_Line (To_Hex_String (Cryptography.HMAC_SHA512 ("mustafa", "carrot")));
   Put_Line
     (To_Hex_String
        (Cryptography.PBKDF2_SHA512
           (To_Byte_Array ("tutturu"), "blabla", 10)));
end Vaniton;
