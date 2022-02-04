with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Interfaces.C;

with Crypto.Symmetric.Algorithm.SHA256;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA512;
with Crypto.Types;
with Crypto.Types.Random;
with Crypto.Types.Base64;
with SPARKNaCl.Sign;
with SPARKNaCl;

with fastpbkdf2_h; use fastpbkdf2_h;
with hmac_sha_c;

package body Cryptography is
   function Pad
     (Item : Crypto.Types.Bytes; Target : Natural) return Crypto.Types.Bytes
   is
      Result : Crypto.Types.Bytes (1 .. Target) := (others => 0);
   begin
      Result (1 .. Item'Length) := Item;
      return Result;
   end Pad;

   function HMAC_SHA512 (Phrase : String; Password : String) return Byte_Array
   is
      use Interfaces.C;

      Phrase_Bytes   : aliased Byte_Array := To_Byte_Array (Phrase);
      Password_Bytes : aliased Byte_Array := To_Byte_Array (Password);

      Result : aliased Byte_Array (1 .. 64);
   begin
      if Password'Length /= 0 then
         Password_Bytes := To_Byte_Array (Password);
      end if;

      if not hmac_sha_c.hmac_sha512
          (Phrase_Bytes (Phrase_Bytes'First)'Access, Phrase_Bytes'Length,
           (if Password'Length /= 0 then
              Password_Bytes (Password_Bytes'First)'Access
            else null),
           Password'Length, Result (1)'Access, Result'Length)
      then
         raise Program_Error;
      end if;

      return Result;
   end HMAC_SHA512;

   function PBKDF2_SHA512
     (Key : Byte_Array; Salt : String; Iterations : Positive) return Byte_Array
   is
      Salt_Bytes : aliased Byte_Array := To_Byte_Array (Salt);
      Key_Copy   : aliased Byte_Array := Key;

      Result : aliased Byte_Array (1 .. 64);
   begin
      fastpbkdf2_hmac_sha512
        (Key_Copy (Key_Copy'First)'Access, Key_Copy'Length,
         Salt_Bytes (Salt_Bytes'First)'Access, Salt_Bytes'Length,
         Unsigned_32 (Iterations), Result (1)'Access, Result'Length);
      return Result;
   end PBKDF2_SHA512;

   function SHA256 (Data : Byte_Array) return Byte_Array is
      use Crypto.Symmetric.Algorithm.SHA256;
      use Crypto.Types;

      subtype Input_Bytes is Bytes (1 .. Data'Length);
      function To_Bytes is new Ada.Unchecked_Conversion
        (Byte_Array, Input_Bytes);

      subtype Output_Type is Byte_Array (1 .. 32);
      function To_Byte_Array is new Ada.Unchecked_Conversion
        (Bytes, Output_Type);

      Result : W_Block256;
   begin
      Hash (To_Bytes (Data), Result);
      return To_Byte_Array (To_Bytes (Result));
   end SHA256;

   function From_Seed (Seed : Byte_Array) return Key_Pair is
      use SPARKNaCl.Sign;
      use SPARKNaCl;

      subtype PK_Type is Byte_Array (1 .. 32);
      subtype SK_Type is Byte_Array (1 .. 64);

      Result : Key_Pair;

      PK : Signing_PK;
      SK : Signing_SK;

      function To_Bytes_32 is new Ada.Unchecked_Conversion
        (Byte_Array, Bytes_32);
      function Convert is new Ada.Unchecked_Conversion (Signing_PK, PK_Type);
      function Convert is new Ada.Unchecked_Conversion (Signing_SK, SK_Type);
   begin
      Keypair (To_Bytes_32 (Seed), PK, SK);

      Result.Public_Key := Convert (PK);
      Result.Secret_Key := Convert (SK);

      return Result;
   end From_Seed;

   function Is_Basic_Seed (Entropy : Byte_Array) return Boolean is
     (PBKDF2_SHA512
        (Entropy, "TON seed version",
         Natural'Max
           (1, Natural (Float'Floor (Float (PBKDF_ITERATIONS) / 256.0))))
        (1) =
      0);
   function Is_Password_Seed (Entropy : Byte_Array) return Boolean is
     (PBKDF2_SHA512 (Entropy, "TON fast seed version", 1) (1) = 1);
   function Is_Password_Needed (Entropy : Byte_Array) return Boolean is
     (Is_Password_Seed (Entropy) and not Is_Basic_Seed (Entropy));
end Cryptography;
