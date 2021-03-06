with Interfaces; use Interfaces;

with Types; use Types;

package Cryptography is
   PBKDF_ITERATIONS : constant Natural := 100_000;
   type Key_Pair is record
      Public_Key : Byte_Array (1 .. 32);
      Secret_Key : Byte_Array (1 .. 64);
   end record;

   function HMAC_SHA512 (Phrase : String; Password : String) return Byte_Array;
   function PBKDF2_SHA512
     (Key : Byte_Array; Salt : String; Iterations : Positive)
      return Byte_Array;

   function SHA256 (Data : Byte_Array) return Byte_Array;

   function From_Seed (Seed : Byte_Array) return Key_Pair;

   function Get_Random return Unsigned_32;
   function Get_Random (Length : Positive) return Unsigned_16_Array;
end Cryptography;
