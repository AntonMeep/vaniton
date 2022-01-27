pragma Ada_2012;

with Ada.Unchecked_Conversion;

with Base64; use Base64;

package body Addresses is
   function Create (Addr : String) return Address is
      Result : Address;
   begin
      Create (Result, Addr);
      return Result;
   end Create;

   procedure Create (This : in out Address; Addr : String) is
      Data : Byte_Array := From_Base64 (Addr);

      function Convert is new Ada.Unchecked_Conversion (Unsigned_8, Signed_8);
   begin
      if Data'Length /= 36 then
         raise Address_Error
           with "Unknown address type: byte length is not equal to 36";
      end if;

      declare
         Address_Part   : Byte_Array := Data (1 .. 34);
         CRC            : Byte_Array := Data (34 .. 36);
         Calculated_CRC : Byte_Array := CRC16 (Address_Part);
         Tag            : Unsigned_8 := Address_Part (1);
         Workchain      : Signed_8   := Convert (Address_Part (2));

         Test_Only  : Boolean := False;
         Bounceable : Boolean := False;
      begin
         if CRC /= Calculated_CRC then
            raise Address_Error with "Wrong CRC16 checksum";
         end if;

         if (Tag and Unsigned_8 (16#80#)) = 1 then
            Test_Only := True;
            Tag       := Tag and not Unsigned_8 (16#80#);
         end if;

         if Tag /= Unsigned_8 (16#11#) and then Tag /= Unsigned_8 (16#51#) then
            raise Address_Error with "Unknown address tag";
         end if;

         Bounceable := Tag = Unsigned_8 (16#11#);

         if Workchain /= 0 and then Workchain /= -1 then
            raise Address_Error with "Invalid address workchain";
         end if;

         This.Test_Only  := Test_Only;
         This.Bounceable := Bounceable;
         This.Workchain  := Workchain;
         This.Hash_Part  := Address_Part (2 .. 34);
         This.CRC        := CRC;
      end;
   end Create;

   function Create
     (Test_Only : Boolean; Bounceable : Boolean; Workchain : Signed_8;
      Hash_Part : Byte_Array (1 .. 32)) return Address
   is
      Result : Address;
   begin
      Create (Result, Test_Only, Bounceable, Workchain, Hash_Part);
      return Result;
   end Create;

   procedure Create
     (This      : in out Address; Test_Only : Boolean; Bounceable : Boolean;
      Workchain :        Signed_8; Hash_Part : Byte_Array (1 .. 32))
   is
   begin
      This.Test_Only  := Test_Only;
      This.Bounceable := Bounceable;
      This.Workchain  := Workchain;
      This.Hash_Part  := Hash_Part;
      This.CRC        := CRC (This);
   end Create;

   function Is_Valid (Addr : String) return Boolean is
      Temporary : Address;
   begin
      Create (Temporary, Addr);
      return True;
   exception
      when others =>
         return False;
   end Is_Valid;

   function Is_Bounceable (This : in Address) return Boolean is
     (This.Bounceable);
   function Is_Test_Only (This : in Address) return Boolean is
     (This.Test_Only);

   function To_String (This : Address) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_String unimplemented");
      return raise Program_Error with "Unimplemented function To_String";
   end To_String;
end Addresses;
