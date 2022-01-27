pragma Ada_2012;
package body Cells is
   function From_BoC (Data : String) return Cell is
     (From_BoC (From_Hex_String (Data)));

   function From_BoC (Data : Byte_Array) return Cell is
   begin
      pragma Compile_Time_Warning (Standard.True, "From_BoC unimplemented");
      return raise Program_Error with "Unimplemented function From_BoC";
   end From_BoC;

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

   procedure Write (This : in out Cell; Data : Cell) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   procedure Write (This : in out Cell; Data : Bit_Array) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   procedure Write (This : in out Cell; Data : Byte_Array) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   procedure Write (This : in out Cell; Data : Unsigned_32) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;
end Cells;
