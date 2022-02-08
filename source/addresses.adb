pragma Ada_2012;

with Ada.Unchecked_Conversion;

with Base64; use Base64;

package body Addresses is
   function Convert is new Ada.Unchecked_Conversion (Integer_8, Unsigned_8);

   function Get_Tag
     (Test_Only : Boolean; Bounceable : Boolean) return Unsigned_8
   is
      Tag : Unsigned_8 := 0;
   begin
      if Test_Only then
         Tag := Unsigned_8 (16#80#);
      end if;

      if Bounceable then
         Tag := Tag or Unsigned_8 (16#11#);
      else
         Tag := Tag or Unsigned_8 (16#51#);
      end if;
      return Tag;
   end Get_Tag;

   function CRC16 (Data : Address) return Byte_Array is
      Binary_Data : Byte_Array (1 .. 34);
   begin

      Binary_Data (1)       := Get_Tag (Data.Test_Only, Data.Bounceable);
      Binary_Data (2)       := Convert (Data.Workchain);
      Binary_Data (3 .. 34) := Data.Hash_Part;
      return CRC16 (Binary_Data);
   end CRC16;

   function Create (Addr : String) return Address is
      Result : Address;
   begin
      Create (Result, Addr);
      return Result;
   end Create;

   procedure Create (This : in out Address; Addr : String) is
      Data : Byte_Array := From_Base64 (Addr);

      function Convert is new Ada.Unchecked_Conversion (Unsigned_8, Integer_8);
   begin
      if Data'Length /= 36 then
         raise Address_Error
           with "Unknown address type: byte length is not equal to 36";
      end if;

      declare
         Address_Part   : Byte_Array := Data (1 .. 34);
         CRC            : Byte_Array := Data (35 .. 36);
         Calculated_CRC : Byte_Array := CRC16 (Address_Part);
         Tag            : Unsigned_8 := Address_Part (1);
         Workchain      : Integer_8  := Convert (Address_Part (2));

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
         This.Hash_Part  := Address_Part (3 .. 34);
         This.CRC        := CRC;
      end;
   end Create;

   function Create
     (Test_Only : Boolean; Bounceable : Boolean; Workchain : Integer_8;
      Hash_Part : Byte_Array) return Address
   is
      Result : Address;
   begin
      Create (Result, Test_Only, Bounceable, Workchain, Hash_Part);
      return Result;
   end Create;

   procedure Create
     (This      : in out Address; Test_Only : Boolean; Bounceable : Boolean;
      Workchain :        Integer_8; Hash_Part : Byte_Array)
   is
   begin
      This.Test_Only  := Test_Only;
      This.Bounceable := Bounceable;
      This.Workchain  := Workchain;
      This.Hash_Part  := Hash_Part;
      This.CRC        := CRC16 (This);
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

   function To_String
     (This      : Address; User_Friendly : Boolean := True;
      Url_Safe  : Boolean    := True; Bounceable : Triboolean := Indeterminate;
      Test_Only : Triboolean := Indeterminate) return String
   is
      Data : Byte_Array (1 .. 36);
   begin
      if User_Friendly then
         Data (1) :=
           Get_Tag
             ((if Test_Only = Indeterminate then This.Test_Only
               else To_Boolean (Test_Only)),
              (if Bounceable = Indeterminate then This.Bounceable
               else To_Boolean (Bounceable)));
         Data (2)        := Convert (This.Workchain);
         Data (3 .. 34)  := This.Hash_Part;
         Data (35 .. 36) := This.CRC;

         if Url_Safe then
            return To_Base64Url (Data);
         else
            return To_Base64 (Data);
         end if;
      else
         return This.Workchain'Image & ":" & To_Hex_String (This.Hash_Part);
      end if;

   end To_String;
end Addresses;
