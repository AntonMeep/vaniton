with Types;      use Types;
with Interfaces; use Interfaces;

package Addresses is
   type Address is private;
   Address_Error : exception;

   function Create (Addr : String) return Address;
   procedure Create (This : in out Address; Addr : String);
   function Create
     (Test_Only : Boolean; Bounceable : Boolean; Workchain : Integer_8;
      Hash_Part : Byte_Array) return Address;
   procedure Create
     (This      : in out Address; Test_Only : Boolean; Bounceable : Boolean;
      Workchain :        Integer_8; Hash_Part : Byte_Array);

   function Is_Valid (Addr : String) return Boolean;
   function Is_Bounceable (This : in Address) return Boolean;
   function Is_Test_Only (This : in Address) return Boolean;

   function To_String (This : Address) return String;
private
   type Address is record
      Test_Only  : Boolean := False;
      Bounceable : Boolean := False;
      Workchain  : Integer_8;
      Hash_Part  : Byte_Array (1 .. 32);
      CRC        : Byte_Array (1 .. 2);
   end record;
end Addresses;
