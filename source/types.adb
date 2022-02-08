pragma Ada_2012;

with Ada.Unchecked_Conversion;

package body Types is
   function To_Triboolean (Value : Boolean) return Triboolean is
     (if Value then Triboolean'(True) else Triboolean'(False));
   function To_Boolean (Value : Triboolean) return Boolean is
     (Value = Triboolean'(True));

   function To_Byte_Array (Item : in String) return Byte_Array is
      Result : Byte_Array (Item'First .. Item'Last);
   begin
      for I in Item'Range loop
         Result (I) := Character'Pos (Item (I));
      end loop;

      return Result;
   end To_Byte_Array;

   function To_String (Item : in Byte_Array) return String is
      Result : String (Item'First .. Item'Last);
   begin
      for I in Item'Range loop
         Result (I) := Character'Val (Item (I));
      end loop;

      return Result;
   end To_String;

   function To_Byte_Array (Item : in Bit_Array) return Byte_Array is
      Result : Byte_Array (1 .. Item'Length / 8) := (others => 0);
      Index  : Positive                          := 1;
      Chunk  : Positive                          := 1;
   begin
      if (Item'Length rem 8) /= 0 then
         raise Program_Error;
      end if;

      while Chunk < Item'Length loop
         Result (Index) :=
           Unsigned_8 (2#1000_0000#) *
           Unsigned_8 (if Item (Chunk) then 1 else 0) +
           Unsigned_8 (2#0100_0000#) *
             Unsigned_8 (if Item (Chunk + 1) then 1 else 0) +
           Unsigned_8 (2#0010_0000#) *
             Unsigned_8 (if Item (Chunk + 2) then 1 else 0) +
           Unsigned_8 (2#0001_0000#) *
             Unsigned_8 (if Item (Chunk + 3) then 1 else 0) +
           Unsigned_8 (2#0000_1000#) *
             Unsigned_8 (if Item (Chunk + 4) then 1 else 0) +
           Unsigned_8 (2#0000_0100#) *
             Unsigned_8 (if Item (Chunk + 5) then 1 else 0) +
           Unsigned_8 (2#0000_0010#) *
             Unsigned_8 (if Item (Chunk + 6) then 1 else 0) +
           Unsigned_8 (2#0000_0001#) *
             Unsigned_8 (if Item (Chunk + 7) then 1 else 0);

         Index := Index + 1;
         Chunk := Chunk + 8;
      end loop;

      return Result;
   end To_Byte_Array;

   function To_Bit_Array (Item : in Byte_Array) return Bit_Array is
      Result : Bit_Array (1 .. Item'Length * 8) := (others => False);

      Chunk : Positive := 1;
   begin
      for Byte of Item loop
         Result (Chunk + 0) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#1000_0000#)) /= 0;
         Result (Chunk + 1) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0100_0000#)) /= 0;
         Result (Chunk + 2) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0010_0000#)) /= 0;
         Result (Chunk + 3) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0001_0000#)) /= 0;
         Result (Chunk + 4) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0000_1000#)) /= 0;
         Result (Chunk + 5) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0000_0100#)) /= 0;
         Result (Chunk + 6) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0000_0010#)) /= 0;
         Result (Chunk + 7) :=
           (Unsigned_8 (Byte) and Unsigned_8 (2#0000_0001#)) /= 0;
         Chunk := Chunk + 8;
      end loop;

      return Result;
   end To_Bit_Array;

   function Padded_Length (Length : in Natural) return Natural is
     (Natural (Float'Ceiling (Float (Length) / Float (8))) * 8);

   function Pad (Item : in Bit_Array) return Bit_Array is
      Result : Bit_Array (1 .. Padded_Length (Item'Length)) :=
        (others => False);
   begin
      Result (1 .. Item'Length) := Item;
      return Result;
   end Pad;

   function To_Hex_String (Item : in Byte_Array) return String is
      To_Hex_Digit : constant array (Unsigned_8 range 0 .. 15) of Character :=
        "0123456789abcdef";
      Result : String (1 .. Item'Length * 2);
      Index  : Positive := 1;
   begin
      for Element of Item loop
         Result (Index)     := To_Hex_Digit (Shift_Right (Element, 4));
         Result (Index + 1) := To_Hex_Digit (Element and 16#0F#);

         Index := Index + 2;
      end loop;

      return Result;
   end To_Hex_String;

   function From_Hex_String (Item : in String) return Byte_Array is
      Result : Byte_Array (1 .. Item'Length / 2) := (others => Unsigned_8 (0));
      I      : Natural                           := Result'First;
   begin
      for C in Item'Range loop
         declare
            Value : Unsigned_8;
         begin
            case Item (C) is
               when '0' .. '9' =>
                  Value :=
                    Unsigned_8
                      (Character'Pos (Item (C)) - Character'Pos ('0'));
               when 'a' .. 'f' =>
                  Value :=
                    Unsigned_8
                      (Character'Pos (Item (C)) - Character'Pos ('a') + 10);
               when 'A' .. 'F' =>
                  Value :=
                    Unsigned_8
                      (Character'Pos (Item (C)) - Character'Pos ('A') + 10);
               when others =>
                  raise Program_Error;
            end case;

            Result (I) := Shift_Left (Result (I), 4) or Value;
         end;

         if (C mod 2) = 0 then
            I := I + 1;
         end if;
      end loop;

      return Result;
   end From_Hex_String;

   function To_Hex_String (Item : in Bit_Array) return String is
     (To_Hex_String (To_Byte_Array (Item)));
   function From_Hex_String (Item : in String) return Bit_Array is
     (To_Bit_Array (From_Hex_String (Item)));

   function Reverse_Bytes (Item : in Byte_Array) return Byte_Array is
      Result : Byte_Array (Item'First .. Item'Last);
   begin
      for I in Item'Range loop
         Result (Result'Last - I + 1) := Item (I);
      end loop;
      return Result;
   end Reverse_Bytes;

   function CRC16 (Data : Byte_Array) return Unsigned_16 is
      Result : Unsigned_16 := 0;
   begin
      for Byte of Data loop
         Result := Result xor Shift_Left (Unsigned_16 (Byte), 8);

         for I in 1 .. 8 loop
            if (Result and Unsigned_16 (16#8000#)) /= 0 then
               Result := Shift_Left (Result, 1) xor Unsigned_16 (16#1021#);
            else
               Result := Shift_Left (Result, 1);
            end if;
         end loop;
      end loop;

      return Result;
   end CRC16;

   function CRC16 (Data : Byte_Array) return Byte_Array is
      subtype Output_Type is Byte_Array (1 .. 2);
      function Convert is new Ada.Unchecked_Conversion
        (Unsigned_16, Output_Type);
   begin
      return Reverse_Bytes (Convert (CRC16 (Data)));
   end CRC16;
end Types;
