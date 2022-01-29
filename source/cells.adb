pragma Ada_2012;

with Ada.Unchecked_Conversion;

with Cryptography;

package body Cells is
   function From_Bits (Data : String) return Cell is
     (From_Bits (From_Hex_String (Data)));

   function From_Bits (Data : Bit_Array) return Cell is
      Result : Cell;
   begin
      Result.Bits (1 .. Data'Length) := Data;
      Result.Length                  := Data'Length;
      return Result;
   end From_Bits;

   function Get_Max_Level (This : Cell) return Unsigned_8 is
      Max_Level : Unsigned_8 := 0;
   begin
      for I in 1 .. This.Number_Of_References loop
         declare
            Inner_Level : Unsigned_8 :=
              Get_Max_Level (This.References (I).all);
         begin
            if Inner_Level > Max_Level then
               Max_Level := Inner_Level;
            end if;
         end;
      end loop;
      return Max_Level;
   end Get_Max_Level;

   function Get_Max_Depth (This : Cell) return Unsigned_16 is
      Max_Depth : Unsigned_16 := 0;
   begin
      if This.Number_Of_References /= 0 then
         for I in 1 .. This.Number_Of_References loop
            declare
               Inner_Depth : Unsigned_16 :=
                 Get_Max_Depth (This.References (I).all);
            begin
               if Inner_Depth > Max_Depth then
                  Max_Depth := Inner_Depth;
               end if;
            end;
         end loop;
         Max_Depth := Max_Depth + 1;
      end if;
      return Max_Depth;
   end Get_Max_Depth;

   function Representation (This : Cell) return Byte_Array is
      Index : Natural := 1;

      Refs_Descriptor : Unsigned_8 :=
        Unsigned_8 (This.Number_Of_References) +
      -- This.Is_Exotic * 8 + -- Completely useless for our purposes
      Get_Max_Level (This) * 32; -- Also does nothing, always 0

      Bits_Descriptor : Unsigned_8 :=
        Unsigned_8 (Float'Ceiling (Float (This.Length) / Float (8))) +
        Unsigned_8 (Float'Floor (Float (This.Length) / Float (8)));

      Data_Length : Natural := Padded_Length (This.Length) / 8;

      Result : Byte_Array
        (1 ..
             2 + Data_Length + 2 * This.Number_Of_References +
             32 * This.Number_Of_References) :=
        (others => 0);
   begin
      Result (Index) := Refs_Descriptor;
      Index          := Index + 1;
      Result (Index) := Bits_Descriptor;
      Index          := Index + 1;

      declare
         Padded : Bit_Array (1 .. Padded_Length (This.Length)) :=
           Pad (This.Bits (1 .. This.Length));
      begin
         if Padded'Length /= This.Length then
            Padded (This.Length + 1) := True;
         end if;
         Result (Index .. Index + Data_Length - 1) := To_Byte_Array (Padded);
         Index                                     := Index + Data_Length;
      end;

      for I in 1 .. This.Number_Of_References loop
         declare
            Max_Depth : Unsigned_16 := Get_Max_Depth (This.References (I).all);
         begin
            Result (Index)     := Unsigned_8 (Max_Depth and 16#FF#);
            Result (Index + 1) := Unsigned_8 (Shift_Right (Max_Depth, 8));
            Index              := Index + 2;
         end;
      end loop;

      for I in 1 .. This.Number_Of_References loop
         Result (Index .. Index + 31) := Hash (This.References (I).all);
         Index                        := Index + 32;
      end loop;
      return Result;
   end Representation;

   function Hash (This : Cell) return Byte_Array is
     (Cryptography.SHA256 (Representation (This)));

   procedure Append_Reference
     (This : in out Cell; Other : not null Cell_Access)
   is
   begin
      if This.Number_Of_References = 4 then
         raise Program_Error with "Maximum number of references reached";
      end if;
      This.Number_Of_References := This.Number_Of_References + 1;
      This.References (This.Number_Of_References) := Other;
   end Append_Reference;

   procedure Write (This : in out Cell; Data : Cell) is
   begin
      Write (This, Data.Bits (1 .. Data.Length));
   end Write;

   procedure Write (This : in out Cell; Data : Bit_Array) is
   begin
      This.Bits (This.Length + 1 .. This.Length + Data'Length) := Data;
      This.Length := This.Length + Data'Length;
   end Write;

   procedure Write (This : in out Cell; Data : Byte_Array) is
   begin
      Write (This, To_Bit_Array (Data));
   end Write;

   procedure Write (This : in out Cell; Data : Unsigned_32) is
      subtype Output_Type is Byte_Array (1 .. 4);
      function Convert is new Ada.Unchecked_Conversion
        (Unsigned_32, Output_Type);
   begin
      Write (This, Convert (Data));
   end Write;

   function Bits (This : in Cell) return Bit_Array is
     (This.Bits (1 .. This.Length));
end Cells;
