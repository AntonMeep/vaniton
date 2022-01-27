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

   function From_Seed (Seed : Byte_Array) return Key_Pair;

   procedure Get_Random_Values (Result : out Unsigned_32_Array);

   function Is_Basic_Seed (Entropy : Byte_Array) return Boolean;
   function Is_Password_Seed (Entropy : Byte_Array) return Boolean;
   function Is_Password_Needed (Entropy : Byte_Array) return Boolean;
end Cryptography;
