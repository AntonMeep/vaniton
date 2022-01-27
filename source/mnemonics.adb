with Interfaces; use Interfaces;

package body Mnemonics is
   function Generate
     (Words_Count : Positive := 24; Password : String := "";
      List        : Wordlist := English_Words) return Mnemonic
   is
      Result : Mnemonic (1 .. Words_Count);
      Random : Unsigned_32_Array (1 .. Words_Count);
   begin
      <<Try_Again>>
      Get_Random_Values (Random);
      for I in Result'Range loop
         Result (I) := List (Natural (Random (I) and Unsigned_32 (List'Last)));
      end loop;

      declare
         Entropy : constant Byte_Array := To_Entropy (Result, Password);
      begin
         if Password'Length > 0 and then not Is_Password_Needed (Entropy) then
            goto Try_Again;
         end if;

         if not Is_Basic_Seed (Entropy) then
            goto Try_Again;
         end if;
      end;

      return Result;
   end Generate;

   function Join (This : Mnemonic) return String is
      use Words.Bounded_Words;
      Phrase_Length : Positive := This'Length - 1;
   begin
      for Word of This loop
         Phrase_Length := Phrase_Length + Length (Word);
      end loop;

      declare
         Result : String (1 .. Phrase_Length);
         I      : Positive := 1;
      begin
         for Word of This loop
            for C in 1 .. Length (Word) loop
               Result (I) := Element (Word, C);
               I          := I + 1;
            end loop;
            if I /= Phrase_Length then
               Result (I) := ' ';
               I          := I + 1;
            end if;
         end loop;
         return Result;
      end;
   end Join;

   function To_Entropy
     (This : Mnemonic; Password : String := "") return Byte_Array is
     (HMAC_SHA512 (Join (This), Password));

   function To_Seed
     (This : Mnemonic; Password : String := "") return Byte_Array is
     (PBKDF2_SHA512
        (To_Entropy (This, Password), "TON default seed", PBKDF_ITERATIONS)
        (1 .. 32));

   function To_Key_Pair
     (This : Mnemonic; Password : String := "") return Key_Pair is
     (From_Seed (To_Seed (This, Password)));
end Mnemonics;
