with Interfaces; use Interfaces;

package Types is
   type Triboolean is (True, False, Indeterminate);
   function To_Triboolean (Value : Boolean) return Triboolean;
   function To_Boolean (Value : Triboolean) return Boolean;

   type Bit_Array is array (Natural range <>) of Boolean;
   pragma Pack (Bit_Array);
   type Byte_Array is array (Natural range <>) of aliased Unsigned_8;

   type Unsigned_32_Array is array (Natural range <>) of Unsigned_32;

   function To_Byte_Array (Item : in String) return Byte_Array;
   function To_String (Item : in Byte_Array) return String;

   function To_Byte_Array (Item : in Bit_Array) return Byte_Array;
   function To_Bit_Array (Item : in Byte_Array) return Bit_Array;

   function Padded_Length (Length : in Natural) return Natural;
   function Pad (Item : in Bit_Array) return Bit_Array;

   function To_Hex_String (Item : in Byte_Array) return String;
   function From_Hex_String (Item : in String) return Byte_Array with
      Pre => (Item'Length mod 2) = 0;

   function To_Hex_String (Item : in Bit_Array) return String;
   function From_Hex_String (Item : in String) return Bit_Array;

   function CRC16 (Data : Byte_Array) return Unsigned_16;
end Types;
