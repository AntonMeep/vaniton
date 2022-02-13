with Ada.Strings.Fixed;

with Interfaces; use Interfaces;

package body Mnemonics is
   function Generate
     (Words_Count : Positive := 24; Password : String := "";
      List        : Wordlist := English_Words) return Mnemonic
   is
      Result : Mnemonic (1 .. Words_Count);

      function Check_Validity return Boolean;

      function Check_Validity return Boolean is
         Entropy : constant Byte_Array := To_Entropy (Result);
      begin
         if Password'Length > 0 and then not Is_Password_Needed (Entropy) then
            return False;
         end if;

         return Is_Basic_Seed (Entropy);
      end Check_Validity;
   begin
      loop
         declare
            Random : constant Unsigned_16_Array := Get_Random (Words_Count);
         begin
            for I in Result'Range loop
               Result (I) :=
                 List (Natural (Random (I) and Unsigned_16 (List'Last)));
            end loop;
         end;

         exit when Check_Validity;
      end loop;

      if not Is_Valid (Result, Password, List) then
         raise Program_Error; -- Debugging only
      end if;

      return Result;
   end Generate;

   function From_String (Input : String) return Mnemonic is
      use Ada.Strings.Fixed;
      use Words.Bounded_Words;

      Result  : Mnemonic (1 .. Count (Input, " ") + 1);
      Current : Positive := 1;
   begin
      for I in Input'Range loop
         if Input (I) = ' ' then
            Current := Current + 1;
         else
            Append (Result (Current), Input (I));
         end if;
      end loop;

      return Result;
   end From_String;

   function To_String (This : Mnemonic) return String is
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
            if I < Phrase_Length then
               Result (I) := ' ';
               I          := I + 1;
            end if;
         end loop;
         return Result;
      end;
   end To_String;

   function Is_Valid
     (This : Mnemonic; Password : String := "";
      List : Wordlist := English_Words) return Boolean
   is
   begin
      for Word of This loop
         declare
            use Words.Bounded_Words;
            Found : Boolean := False;
         begin
            for List_Word of List loop
               if Word = List_Word then
                  Found := True;
               end if;
               exit when Found;
            end loop;

            if not Found then
               return False;
            end if;
         end;
      end loop;

      declare
         Entropy : constant Byte_Array := To_Entropy (This, Password);
      begin
         if Password'Length > 0 and then not Is_Password_Needed (Entropy) then
            return False;
         end if;

         return Is_Basic_Seed (Entropy);
      end;
   end Is_Valid;

   function To_Entropy
     (This : Mnemonic; Password : String := "") return Byte_Array is
     (HMAC_SHA512 (To_String (This), Password));

   function To_Seed
     (This : Mnemonic; Password : String := "") return Byte_Array is
     (PBKDF2_SHA512
        (To_Entropy (This, Password), "TON default seed", PBKDF_ITERATIONS)
        (1 .. 32));

   function To_Key_Pair
     (This : Mnemonic; Password : String := "") return Key_Pair is
     (From_Seed (To_Seed (This, Password)));
end Mnemonics;
