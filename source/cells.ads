with Types; use Types;

package Cells is
   type Cell is private;
   type Cell_Access is access Cell;

   function Representation (This : Cell) return Byte_Array;
   function Hash (This : Cell) return Byte_Array;
private
   type Cell is record
      Bits      : Bit_Array (1 .. 1_023);
      Is_Exotic : Boolean := False;
   end record;

end Cells;
