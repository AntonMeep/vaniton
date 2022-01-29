pragma Ada_2012;

with Interfaces; use Interfaces;

package body Base64 is
   PAD      : constant Character := '=';
   DE_FIRST : constant Character := '+';
   DE_LAST  : constant Character := 'z';

   type Character_Array is array (Natural range <>) of Character;
   type Unsigned_8_Array is array (Natural range <>) of Unsigned_8;

   Encoding_Table : constant Character_Array (0 .. 63) :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
   Decoding_Table : constant Unsigned_8_Array (0 .. 127) :=
     (
   -- nul, soh, stx, etx, eot, enq, ack, bel,
   255, 255, 255, 255, 255, 255, 255, 255,
   -- bs,  ht,  nl,  vt,  np,  cr,  so,  si,
   255, 255, 255, 255, 255, 255,
      255, 255,
   -- dle, dc1, dc2, dc3, dc4, nak, syn, etb,
   255, 255, 255, 255, 255, 255, 255, 255,
   -- can,  em, sub, esc,  fs,  gs,  rs,  us,
   255, 255, 255, 255,
      255, 255, 255, 255,
   -- sp, '!', '"', '#', '$', '%', '&', ''',
   255, 255, 255, 255, 255, 255, 255, 255,
   -- '(', ')', '*', '+', ',', '-', '.', '/',
   255, 255,
      255, 62, 255, 255, 255, 63,
   -- '0', '1', '2', '3', '4', '5', '6', '7',
   52, 53, 54, 55, 56, 57, 58, 59,
   -- '8', '9', ':', ';', '<', '=', '>', '?',
   60, 61, 255,
      255, 255, 255, 255, 255,
   -- '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
   255, 0, 1, 2, 3, 4, 5, 6,
   -- 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
   7, 8, 9, 10, 11, 12,
      13, 14,
   -- 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
   15, 16, 17, 18, 19, 20, 21, 22,
   -- 'X', 'Y', 'Z', '[', '\', ']', '^', '_',
   23, 24, 25, 255, 255, 255, 255,
      255,
   -- '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
   255, 26, 27, 28, 29, 30, 31, 32,
   -- 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
   33, 34, 35, 36, 37, 38, 39, 40,
   -- 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
   41,
      42, 43, 44, 45, 46, 47, 48,
   -- 'x', 'y', 'z', '{', '|', '}', '~', del,
   49, 50, 51, 255, 255, 255, 255, 255);

   function From_Base64 (Input : String) return Byte_Array is
      Result : Byte_Array
        (1 .. Input'Length * 2); -- TODO: optimize stack usage here
      Index   : Natural := Input'First;
      Current : Unsigned_8;
   begin
      if (Unsigned_32 (Input'Length) and Unsigned_32 (16#3#)) /= 0 then
         raise Base64_Error with "Invalid base64 string length";
      end if;

      for I in Input'Range loop
         exit when Input (I) = PAD;

         if Input (I) < DE_FIRST or else Input (I) > DE_LAST then
            raise Base64_Error with "Invalid base64 character";
         end if;

         Current := Decoding_Table (Character'Pos (Input (I)));
         if Current = 255 then
            raise Base64_Error with "Invalid base64 character";
         end if;

         case Unsigned_32 (I - Input'First) and Unsigned_32 (16#3#) is
            when 0 =>
               Result (Index) :=
                 Shift_Left (Current, 2) and Unsigned_8 (16#FF#);
            when 1 =>
               Result (Index) :=
                 Result (Index) or
                 (Shift_Right (Current, 4) and Unsigned_8 (16#3#));
               Index          := Index + 1;
               Result (Index) :=
                 Shift_Left (Current and Unsigned_8 (16#F#), 4);
            when 2 =>
               Result (Index) :=
                 Result (Index) or
                 (Shift_Right (Current, 2) and Unsigned_8 (16#F#));
               Index          := Index + 1;
               Result (Index) :=
                 Shift_Left (Current and Unsigned_8 (16#3#), 6);
            when 3 =>
               Result (Index) := Result (Index) or Current;
               Index          := Index + 1;
            when others =>
               raise Program_Error;
         end case;
      end loop;

      return Result (1 .. Index);
   end From_Base64;

   function To_Base64 (Input : Byte_Array) return String is
      Result : String
        (1 .. Input'Length * 2); -- TODO: optimize stack usage here
      Index : Natural    := 0;
      Step  : Natural    := 0;
      Last  : Unsigned_8 := 0;
   begin
      for Current of Input loop
         case Step is
            when 0 =>
               Step           := 1;
               Index          := Index + 1;
               Result (Index) :=
                 Encoding_Table
                   (Integer
                      (Shift_Right (Current, 2) and Unsigned_8 (16#3F#)));
            when 1 =>
               Step           := 2;
               Index          := Index + 1;
               Result (Index) :=
                 Encoding_Table
                   (Integer
                      (Shift_Left (Last and Unsigned_8 (16#3#), 4) or
                       (Shift_Right (Current, 4) and Unsigned_8 (16#F#))));
            when 2 =>
               Step               := 0;
               Result (Index + 1) :=
                 Encoding_Table
                   (Integer
                      (Shift_Left (Last and Unsigned_8 (16#F#), 2) or
                       (Shift_Right (Current, 6) and Unsigned_8 (16#3#))));
               Result (Index + 2) :=
                 Encoding_Table (Integer (Current and Unsigned_8 (16#3F#)));
               Index := Index + 2;
            when others =>
               raise Program_Error;
         end case;
         Last := Current;
      end loop;

      case Step is
         when 1 =>
            Result (Index + 1) :=
              Encoding_Table
                (Integer (Shift_Left (Last and Unsigned_8 (16#3#), 4)));
            Result (Index + 2) := PAD;
            Result (Index + 3) := PAD;
            Index              := Index + 3;
         when 2 =>
            Result (Index + 1) :=
              Encoding_Table
                (Integer (Shift_Left (Last and Unsigned_8 (16#F#), 2)));
            Result (Index + 2) := PAD;
            Index              := Index + 2;
         when others =>
            null;
      end case;

      return Result (1 .. Index);
   end To_Base64;
end Base64;
