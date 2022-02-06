with Ada.Unchecked_Conversion;
with Ada.Streams;
with Interfaces.C.Extensions;

with GNAT.SHA256; use GNAT.SHA256;

with SPARKNaCl.Sign;
with SPARKNaCl;

with fastpbkdf2_h; use fastpbkdf2_h;
with hmac_sha_c;

package body Cryptography is

   function HMAC_SHA512 (Phrase : String; Password : String) return Byte_Array
   is
      use Interfaces.C.Extensions;

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
      use Ada.Streams;

      subtype Buffer_Type is Stream_Element_Array (1 .. Data'Length);
      function To_Stream_Element_Array is new Ada.Unchecked_Conversion
        (Byte_Array, Buffer_Type);

      subtype Output_Type is Byte_Array (1 .. 32);
      function To_Output is new Ada.Unchecked_Conversion
        (Stream_Element_Array, Output_Type);
   begin
      return To_Output (Digest (To_Stream_Element_Array (Data)));
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

   function Get_Random return Unsigned_32 is
      use Interfaces.C;

      type uchar_array is array (Natural range <>) of aliased unsigned_char;
      Buffer : uchar_array (0 .. 3);

      function RAND_bytes
        (buf : access unsigned_char; num : int) return int with
         Import        => True,
         Convention    => C,
         External_Name => "RAND_bytes";
      function To_Unsigned_32 is new Ada.Unchecked_Conversion
        (uchar_array, Unsigned_32);
   begin
      if RAND_bytes (Buffer (0)'Access, Buffer'Length) /= 1 then
         raise Program_Error;
      end if;
      return To_Unsigned_32 (Buffer);
   end Get_Random;
end Cryptography;
