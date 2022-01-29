pragma Ada_2012;

with Ada.Unchecked_Conversion;

package body Types is
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
      subtype Output_Type is Byte_Array (1 .. Item'Length / 8);

      function Convert is new Ada.Unchecked_Conversion
        (Bit_Array, Output_Type);
   begin
      if (Item'Length rem 8) /= 0 then
         raise Program_Error;
      end if;
      return Convert (Item);
   end To_Byte_Array;
   function To_Bit_Array (Item : in Byte_Array) return Bit_Array is
      subtype Output_Type is Bit_Array (1 .. Item'Length * 8);
      function Convert is new Ada.Unchecked_Conversion
        (Byte_Array, Output_Type);
   begin
      return Convert (Item);
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
end Types;
