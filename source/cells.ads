with Interfaces; use Interfaces;

with Types; use Types;

package Cells is
   type Cell is private;
   Empty_Cell : constant Cell;

   function From_Bits (Data : String) return Cell;
   function From_Bits (Data : Bit_Array) return Cell;
   function Representation (This : Cell) return Byte_Array;
   function Hash (This : Cell) return Byte_Array;

   procedure Append_Reference
     (This : in out Cell; Other : not null access Cell);

   procedure Write (This : in out Cell; Data : Cell);
   procedure Write (This : in out Cell; Data : Bit_Array);
   procedure Write (This : in out Cell; Data : Byte_Array);
   procedure Write (This : in out Cell; Data : Unsigned_32);
private
   type Cell is record
      Bits   : Bit_Array (1 .. 1_023);
      Length : Natural := 0;
   end record;

   Empty_Cell : constant Cell := (Bits => (others => False), Length => 0);
end Cells;
