pragma Ada_2012;

with Ada.Unchecked_Conversion;

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

   function Representation (This : Cell) return Byte_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Representation unimplemented");
      return raise Program_Error with "Unimplemented function Representation";
   end Representation;

   function Hash (This : Cell) return Byte_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Hash unimplemented");
      return raise Program_Error with "Unimplemented function Hash";
   end Hash;

   procedure Append_Reference
     (This : in out Cell; Other : not null access Cell)
   is
   begin
      raise Program_Error with "Unimplemented function Append";
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
