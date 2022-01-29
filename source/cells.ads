with Interfaces; use Interfaces;

with Types; use Types;

package Cells is
   type Cell is private;
   type Cell_Access is access all Cell;
   Empty_Cell : constant Cell;

   function From_Bits (Data : String) return Cell;
   function From_Bits (Data : Bit_Array) return Cell;
   function Representation (This : Cell) return Byte_Array;
   function Hash (This : Cell) return Byte_Array;

   procedure Append_Reference
     (This : in out Cell; Other : not null Cell_Access);

   procedure Write (This : in out Cell; Data : Cell);
   procedure Write (This : in out Cell; Data : Bit_Array);
   procedure Write (This : in out Cell; Data : Byte_Array);
   procedure Write (This : in out Cell; Data : Unsigned_32);

   function Bits (This : in Cell) return Bit_Array;
private
   type Cell_Access_Array is array (Positive range 1 .. 4) of Cell_Access;

   type Cell is record
      Bits                 : Bit_Array (1 .. 1_023) := (others => False);
      Length               : Natural                := 0;
      References           : Cell_Access_Array;
      Number_Of_References : Natural                := 0;
   end record;

   Empty_Cell : constant Cell :=
     (Bits => (others => False), Length => 0, References => (others => null),
      Number_Of_References => 0);
end Cells;
