with Ada.Unchecked_Conversion;
with Ada.Streams;
with Interfaces.C;
with Interfaces.C.Extensions;

with System;

with GNAT.SHA256; use GNAT.SHA256;

with SPARKNaCl.Sign;
with SPARKNaCl;

package body Cryptography is

   function HMAC_SHA512 (Phrase : String; Password : String) return Byte_Array
   is
      use System;
      use Interfaces.C;
      use Interfaces.C.Extensions;

      Phrase_Bytes   : aliased Byte_Array := To_Byte_Array (Phrase);
      Password_Bytes : aliased Byte_Array := To_Byte_Array (Password);

      Result  : aliased Byte_Array (1 .. 64);
      Written : aliased size_t := 0;

      function EVP_sha512 return void_ptr with
         Import        => True,
         Convention    => C,
         External_Name => "EVP_sha512";

      function HMAC
        (evp_md  : void_ptr; key : access Interfaces.Unsigned_8;
         key_len : size_t; d : access Interfaces.Unsigned_8; n : size_t;
         md      : access Interfaces.Unsigned_8; md_len : access size_t)
         return void_ptr with
         Import        => True,
         Convention    => C,
         External_Name => "HMAC";
   begin
      if Password'Length /= 0 then
         Password_Bytes := To_Byte_Array (Password);
      end if;

      if HMAC
          (EVP_sha512, Phrase_Bytes (Phrase_Bytes'First)'Access,
           Phrase_Bytes'Length,
           (if Password'Length /= 0 then
              Password_Bytes (Password_Bytes'First)'Access
            else null),
           Password'Length, Result (1)'Access, Written'Access) =
        Null_Address
      then
         raise Program_Error;
      end if;

      if Written /= Result'Length then
         raise Program_Error;
      end if;

      return Result;
   end HMAC_SHA512;

   function PBKDF2_SHA512
     (Key : Byte_Array; Salt : String; Iterations : Positive) return Byte_Array
   is
      use Interfaces.C;

      Salt_Bytes : aliased Byte_Array := To_Byte_Array (Salt);
      Key_Copy   : aliased Byte_Array := Key;

      Result : aliased Byte_Array (1 .. 64);

      procedure fastpbkdf2_hmac_sha512
        (pw    : access Unsigned_8; npw : size_t; salt_1 : access Unsigned_8;
         nsalt : size_t; iterations_1 : Unsigned_32; c_out : access Unsigned_8;
         nout  : size_t) with
         Import        => True,
         Convention    => C,
         External_Name => "fastpbkdf2_hmac_sha512";
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

   use Interfaces.C;
   type uchar_array is array (Natural range <>) of aliased unsigned_char;

   function RAND_bytes (buf : access unsigned_char; num : int) return int with
      Import        => True,
      Convention    => C,
      External_Name => "RAND_bytes";

   function Get_Random return Unsigned_32 is
      Buffer : uchar_array (0 .. 3);

      function To_Unsigned_32 is new Ada.Unchecked_Conversion
        (uchar_array, Unsigned_32);
   begin
      if RAND_bytes (Buffer (0)'Access, Buffer'Length) /= 1 then
         raise Program_Error;
      end if;
      return To_Unsigned_32 (Buffer);
   end Get_Random;

   function Get_Random (Length : Positive) return Unsigned_16_Array is
      subtype Result_Type is Unsigned_16_Array (1 .. Length);

      Buffer : uchar_array (0 .. Length * 2 - 1);

      function To_Unsigned_16_Array is new Ada.Unchecked_Conversion
        (uchar_array, Result_Type);
   begin
      if RAND_bytes (Buffer (0)'Access, Buffer'Length) /= 1 then
         raise Program_Error;
      end if;
      return To_Unsigned_16_Array (Buffer);
   end Get_Random;
end Cryptography;
