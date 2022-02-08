with Ada.Finalization;

with System.Atomic_Counters; use System.Atomic_Counters;

with Interfaces; use Interfaces;

with Types; use Types;

package Cells is
   type Cell is new Ada.Finalization.Controlled with private;

   function Empty_Cell return Cell;

   Cell_Error : exception;

   function From_BoC (Data : String) return Cell;
   function From_BoC (Data : Byte_Array) return Cell;

   function Representation (This : in out Cell) return Byte_Array;
   function Hash (This : in out Cell) return Byte_Array;

   procedure Append_Reference (This : in out Cell; Other : in Cell);

   procedure Write (This : in out Cell; Data : Cell);
   procedure Write (This : in out Cell; Data : Bit_Array);
   procedure Write (This : in out Cell; Data : Byte_Array);
   procedure Write (This : in out Cell; Data : Unsigned_32);
   procedure Write (This : in out Cell; Data : Boolean);
private
   type Cell_Data;
   type Cell_Data_Access is access all Cell_Data;
   type Cell_Data_Array is array (Positive range <>) of Cell_Data_Access;

   type Cell_Data is record
      Reference_Count : aliased Atomic_Unsigned  := 1;
      Bits            : Bit_Array (1 .. 1_023)   := (others => False);
      Length          : Natural                  := 0;
      Cell_References : Cell_Data_Array (1 .. 4) := (others => null);
   end record;

   type Cell is new Ada.Finalization.Controlled with record
      Data : Cell_Data_Access;
   end record;

   procedure Initialize (This : in out Cell);
   procedure Adjust (This : in out Cell);
   procedure Finalize (This : in out Cell);
end Cells;
