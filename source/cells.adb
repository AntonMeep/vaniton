pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Cryptography;

package body Cells is
   function Empty_Cell return Cell is
      Result : Cell;
   begin
      return Result;
   end Empty_Cell;

   function From_BoC (Data : String) return Cell is
     (From_BoC (From_Hex_String (Data)));

   function From_BoC (Data : Byte_Array) return Cell is
      Magic : constant Byte_Array (1 .. 4) := From_Hex_String ("b5ee9c72");

      Index : Positive := 1;

      has_idx, hash_crc32, has_cache_bits : Boolean    := False;
      flags, size_bytes, offset_bytes     : Unsigned_8 := 0;

      cells_num, roots_num, absent_num, tot_cells_size : Unsigned_32;

      function Read_NBytes
        (A : Byte_Array; I : Positive; N : Unsigned_8) return Unsigned_32
      is
         R : Unsigned_32 := 0;
      begin
         for B of A (I .. I + Positive (N) - 1) loop
            R := R * 256;
            R := R + Unsigned_32 (B);
         end loop;
         return R;
      end Read_NBytes;
   begin
      if Data (1 .. 4) /= Magic then
         raise Cell_Error with "Invalid magic number";
      end if;
      Index := Index + 4;

      has_idx        := (Data (Index) and 128) /= 0;
      hash_crc32     := (Data (Index) and 64) /= 0;
      has_cache_bits := (Data (Index) and 32) /= 0;
      flags          := (Data (Index) and 16) * 2 + (Data (Index) and 8);
      size_bytes     := Data (Index) rem 8;
      Index          := Index + 1;

      offset_bytes   := Data (Index);
      Index          := Index + 1;
      cells_num      := Read_NBytes (Data, Index, size_bytes);
      Index          := Index + Positive (size_bytes);
      roots_num      := Read_NBytes (Data, Index, size_bytes);
      Index          := Index + Positive (size_bytes);
      absent_num     := Read_NBytes (Data, Index, size_bytes);
      Index          := Index + Positive (size_bytes);
      tot_cells_size := Read_NBytes (Data, Index, offset_bytes);
      Index          := Index + Positive (offset_bytes);

      declare
         type Root_List_Type is array (Positive range <>) of Unsigned_32;
         root_list : Root_List_Type (1 .. Positive (roots_num));

         type Index_List_Type is array (Positive range <>) of Unsigned_32;
         index_list : Index_List_Type (1 .. Positive (cells_num));

         cells_data : Byte_Array (1 .. Positive (tot_cells_size));

         type Cell_Array_Type is array (Positive range <>) of Cell;
         cells_array : Cell_Array_Type (1 .. Positive (cells_num));

         type Cell_Reference_Array is array (Positive range <>) of Unsigned_32;
         type Cell_Reference_Map is
           array (Positive range <>) of Cell_Reference_Array (1 .. 4);
         cells_reference : Cell_Reference_Map (1 .. Positive (cells_num)) :=
           (others => (others => Unsigned_32'Last));

         Cells_Data_Index : Positive := 1;
      begin
         for I in 1 .. Positive (roots_num) loop
            root_list (I) := Read_NBytes (Data, Index, size_bytes);
            Index         := Index + Positive (size_bytes);
         end loop;

         if has_idx then
            for I in 1 .. Positive (cells_num) loop
               index_list (I) := Read_NBytes (Data, Index, offset_bytes);
               Index          := Index + Positive (offset_bytes);
            end loop;
         end if;

         cells_data := Data (Index .. Index + Positive (tot_cells_size) - 1);
         Index      := Index + Positive (tot_cells_size);

         for I in 1 .. Positive (cells_num) loop
            declare
               D1 : Unsigned_8 := cells_data (Cells_Data_Index);
               D2 : Unsigned_8 := cells_data (Cells_Data_Index + 1);

               Reference_Number : Unsigned_8 := D1 rem 8;
               Data_Bytes_Size  : Unsigned_8 :=
                 Unsigned_8 (Float'Ceiling (Float (D2) / Float (2)));
               Fullfilled_Bytes : Boolean := (D2 rem 2) = 0;
            begin
               Cells_Data_Index := Cells_Data_Index + 2;

               cells_array (I).Data.all.Bits
                 (1 .. Positive (Data_Bytes_Size) * 8) :=
                 To_Bit_Array
                   (cells_data
                      (Cells_Data_Index ..
                           Cells_Data_Index + Positive (Data_Bytes_Size) - 1));
               if Fullfilled_Bytes then
                  cells_array (I).Data.all.Length :=
                    Natural (Data_Bytes_Size) * 8;
               else
                  for L in reverse 1 .. Positive (Data_Bytes_Size) * 8 loop
                     if cells_array (I).Data.all.Bits (L) then
                        cells_array (I).Data.all.Bits (L) := False;
                        cells_array (I).Data.all.Length   := L - 1;
                        exit;
                     end if;
                  end loop;
               end if;
               Cells_Data_Index :=
                 Cells_Data_Index + Positive (Data_Bytes_Size);

               for L in 1 .. Reference_Number loop
                  cells_reference (I) (Integer (L)) :=
                    (Read_NBytes (cells_data, Cells_Data_Index, size_bytes));
                  Cells_Data_Index := Cells_Data_Index + Positive (size_bytes);
               end loop;
            end;
         end loop;

         for I in reverse 1 .. Positive (cells_num) loop
            for R of cells_reference (I) loop
               exit when R = Unsigned_32'Last;
               Append_Reference
                 (cells_array (I), cells_array (Integer (R) + 1));
            end loop;
         end loop;

         if hash_crc32 then
            -- todo we actually don't check shit here
            Index := Index + 4;
         end if;

         if Index /= Data'Length + 1 then
            raise Cell_Error with "Too much data for BoC deserialization";
         end if;

         return cells_array (1);
      end;
   end From_BoC;

   function Get_Max_Level (This : in out Cell_Data) return Unsigned_8 is
      -- Idk why this exists, it always just returns zero duh
      Max_Level : Unsigned_8 := 0;
   begin
      for I in This.Cell_References'Range loop
         exit when This.Cell_References (I) = null;
         declare
            Inner_Level : Unsigned_8 :=
              Get_Max_Level (This.Cell_References (I).all);
         begin
            if Inner_Level > Max_Level then
               Max_Level := Inner_Level;
            end if;
         end;
      end loop;
      return Max_Level;
   end Get_Max_Level;

   function Get_Max_Depth (This : in out Cell_Data) return Unsigned_16 is
      Max_Depth : Unsigned_16 := 0;
   begin
      for I in This.Cell_References'Range loop
         exit when This.Cell_References (I) = null;
         declare
            Inner_Depth : Unsigned_16 :=
              Get_Max_Depth (This.Cell_References (I).all);
         begin
            if Inner_Depth > Max_Depth then
               Max_Depth := Inner_Depth;
            end if;
         end;
      end loop;

      if This.Cell_References (1) /= null then
         Max_Depth := Max_Depth + 1;
      end if;

      return Max_Depth;
   end Get_Max_Depth;

   function Hash (This : in out Cell_Data) return Byte_Array;

   function Representation (This : in out Cell_Data) return Byte_Array is
      Number_Of_References : Natural := 0;
      Index                : Natural := 1;

      Data_Length : constant Natural := Padded_Length (This.Length) / 8;
   begin
      for I in This.Cell_References'Range loop
         exit when This.Cell_References (I) = null;
         Number_Of_References := Number_Of_References + 1;
      end loop;

      declare
         Result : Byte_Array
           (1 ..
                2 + Data_Length + 2 * Number_Of_References +
                32 * Number_Of_References) :=
           (others => 0);

         Padded : Bit_Array (1 .. Padded_Length (This.Length)) :=
           Pad (This.Bits (1 .. This.Length));
      begin
         -- First byte - reference descriptor
         Result (Index) :=
           Unsigned_8 (Number_Of_References) +
         -- This.Is_Exotic * 8 + -- Completely useless for our purposes
         Get_Max_Level (This) * 32; -- Also does nothing, always 0
         Index := Index + 1;
         -- Second byte - bits descriptor
         Result (Index) :=
           Unsigned_8 (Float'Ceiling (Float (This.Length) / Float (8))) +
           Unsigned_8 (Float'Floor (Float (This.Length) / Float (8)));
         Index := Index + 1;

         -- Padded data
         if Padded'Length /= This.Length then
            Padded (This.Length + 1) := True;
         end if;
         Result (Index .. Index + Data_Length - 1) := To_Byte_Array (Padded);
         Index                                     := Index + Data_Length;

         for I in 1 .. Number_Of_References loop
            declare
               Max_Depth : Unsigned_16 :=
                 Get_Max_Depth (This.Cell_References (I).all);
            begin
               Result (Index) :=
                 Unsigned_8 (Float'Floor (Float (Max_Depth) / Float (256)));
               Result (Index + 1) := Unsigned_8 (Max_Depth rem 256);
               Index              := Index + 2;
            end;
         end loop;

         for I in 1 .. Number_Of_References loop
            Result (Index .. Index + 31) :=
              Hash (This.Cell_References (I).all);
            Index := Index + 32;
         end loop;
         return Result;
      end;
   end Representation;

   function Representation (This : in out Cell) return Byte_Array is
     (Representation (This.Data.all));

   function Hash (This : in out Cell_Data) return Byte_Array is
     (Cryptography.SHA256 (Representation (This)));

   function Hash (This : in out Cell) return Byte_Array is
     (Hash (This.Data.all));

   procedure Append_Reference (This : in out Cell; Other : in Cell) is
   begin
      for I in This.Data.all.Cell_References'Range loop
         if This.Data.all.Cell_References (I) = null then
            This.Data.all.Cell_References (I) := Other.Data;
            Increment (Other.Data.all.Reference_Count);
            return;
         end if;
      end loop;
      raise Cell_Error with "Cannot append any more elements to the cell";
   end Append_Reference;

   procedure Write (This : in out Cell; Data : Cell) is
   begin
      Write (This, Data.Data.all.Bits (1 .. Data.Data.all.Length));
   end Write;

   procedure Write (This : in out Cell; Data : Bit_Array) is
   begin
      This.Data.all.Bits
        (This.Data.all.Length + 1 .. This.Data.all.Length + Data'Length) :=
        Data;
      This.Data.all.Length := This.Data.all.Length + Data'Length;
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
      Write (This, Reverse_Bytes (Convert (Data)));
   end Write;

   procedure Write (This : in out Cell; Data : Boolean) is
   begin
      This.Data.all.Bits (This.Data.all.Length + 1) := Data;
      This.Data.all.Length := This.Data.all.Length + 1;
   end Write;

   procedure Initialize (This : in out Cell) is
   begin
      This.Data := new Cell_Data;
   end Initialize;

   procedure Adjust (This : in out Cell) is
   begin
      Increment (This.Data.all.Reference_Count);
   end Adjust;

   procedure Decrement (This : in out Cell_Data_Access) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Cell_Data, Cell_Data_Access);
   begin
      if Decrement (This.all.Reference_Count) then
         for I in This.all.Cell_References'Range loop
            if This.all.Cell_References (I) /= null then
               Decrement (This.all.Cell_References (I));
               This.all.Cell_References (I) := null;
            end if;
         end loop;

         Deallocate (This);
         This := null;
      end if;
   end Decrement;

   procedure Finalize (This : in out Cell) is
   begin
      Decrement (This.Data);
   end Finalize;
end Cells;
