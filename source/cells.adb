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
      Refs_Descriptor : Unsigned_8 :=
        Unsigned_8 (This.Number_Of_References) +
      -- This.Is_Exotic * 8 + -- Completely useless for our purposes
      Get_Max_Level (This) * 32; -- Also does nothing, always 0

      Bits_Descriptor : Unsigned_8 :=
        Shift_Left (Unsigned_8 (This.Length / 8), 4) +
        Shift_Right (Unsigned_8 (This.Length / 8), 4);

      Result : Byte_Array
        (1 ..
             2 + 2 * This.Number_Of_References +
             32 * This.Number_Of_References);
   begin
      Result (1) := Refs_Descriptor;
      Result (2) := Bits_Descriptor;

      -- Actual data somewhere here

      for I in 1 .. This.Number_Of_References loop
         declare
            Max_Depth : Unsigned_16 := Get_Max_Depth (This.References (I).all);
         begin
            Result (I * 2 + 1) := Unsigned_8 (Max_Depth and 16#FF#);
            Result (I * 2 + 2) := Unsigned_8 (Shift_Right (Max_Depth, 8));
         end;
      end loop;

      for I in 1 .. This.Number_Of_References loop
         Result
           (1 + 2 + 2 * This.Number_Of_References + (I - 1) * 32 ..
                2 + 2 * This.Number_Of_References + I * 32) :=
           Hash (This.References (I).all);
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
end Cells;
