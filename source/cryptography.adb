with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA512;
with Crypto.Types;
with Crypto.Types.Random;
with Crypto.Types.Base64;
with SPARKNaCl.Sign;
with SPARKNaCl;

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
      use Crypto.Symmetric.Algorithm.SHA512;
      use Crypto.Symmetric.Mac.Hmac_SHA512;
      use Crypto.Types;

      Phrase_Bytes : Bytes := To_Bytes (Phrase);
      Key          : DW_Block1024;

      Context      : HMAC_Context;
      Result       : DW_Block512;
      Result_Bytes : Bytes (1 .. 64);

      subtype Output_Byte_Array is Byte_Array (1 .. 64);

      function To_Byte_Array is new Ada.Unchecked_Conversion
        (Bytes, Output_Byte_Array);
   begin
      if Phrase_Bytes'Length > 128 then
         declare
            Key_Small : DW_Block512;
         begin
            Hash (Phrase_Bytes, Key_Small);
            Key := To_DW_Block1024 (Pad (To_Bytes (Key_Small), 128));
         end;
      else
         Key := To_DW_Block1024 (Pad (Phrase_Bytes, 128));
      end if;

      Init (Context, Key);
      Final_Sign
        (Context, To_DW_Block1024 (Pad (To_Bytes (Password), 128)),
         Password'Length, Result);
      Result_Bytes := To_Bytes (Result);
      return To_Byte_Array (Result_Bytes);
   end HMAC_SHA512;

   function PBKDF2_SHA512
     (Key : Byte_Array; Salt : String; Iterations : Positive) return Byte_Array
   is
      use Crypto.Types;

      package PBKDF2 is new Crypto.Symmetric.KDF_PBKDF2
        (Hmac_Package    => Crypto.Symmetric.Mac.Hmac_SHA512,
         To_Message_Type => To_DW_Block1024, To_Bytes => To_Bytes,
         "xor"           => "xor");
      use PBKDF2;

      Scheme : PBKDF2_KDF;
      Result : Bytes (1 .. 64);

      subtype Input_Bytes is Bytes (1 .. Key'Length);
      function To_Bytes is new Ada.Unchecked_Conversion
        (Byte_Array, Input_Bytes);

      subtype Output_Byte_Array is Byte_Array (1 .. 64);
      function To_Byte_Array is new Ada.Unchecked_Conversion
        (Bytes, Output_Byte_Array);
   begin
      Initialize (Scheme, Result'Length, Iterations);
      Derive (Scheme, To_Bytes (Salt), To_Bytes (Key), Result);
      return To_Byte_Array (Result);
   end PBKDF2_SHA512;

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

   procedure Get_Random_Values (Result : out Unsigned_32_Array) is
      use Crypto.Types;
      use Crypto.Types.Random;
      Value : Word;

      function Convert is new Ada.Unchecked_Conversion (Word, Unsigned_32);
   begin
      for I in Result'Range loop
         Read (Value);
         Result (I) := Convert (Value);
      end loop;
   end Get_Random_Values;

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
