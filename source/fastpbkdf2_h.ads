pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

package fastpbkdf2_h is

   procedure fastpbkdf2_hmac_sha1
     (pw    : access Unsigned_8; npw : size_t; salt : access Unsigned_8;
      nsalt : size_t; iterations : Unsigned_32; c_out : access Unsigned_8;
      nout  : size_t)  -- .\third-party\fastpbkdf2\fastpbkdf2.h:34
   with
      Import        => True,
      Convention    => C,
      External_Name => "fastpbkdf2_hmac_sha1";

   procedure fastpbkdf2_hmac_sha256
     (pw    : access Unsigned_8; npw : size_t; salt : access Unsigned_8;
      nsalt : size_t; iterations : Unsigned_32; c_out : access Unsigned_8;
      nout  : size_t)  -- .\third-party\fastpbkdf2\fastpbkdf2.h:48
   with
      Import        => True,
      Convention    => C,
      External_Name => "fastpbkdf2_hmac_sha256";

   procedure fastpbkdf2_hmac_sha512
     (pw    : access Unsigned_8; npw : size_t; salt : access Unsigned_8;
      nsalt : size_t; iterations : Unsigned_32; c_out : access Unsigned_8;
      nout  : size_t)  -- .\third-party\fastpbkdf2\fastpbkdf2.h:62
   with
      Import        => True,
      Convention    => C,
      External_Name => "fastpbkdf2_hmac_sha512";

end fastpbkdf2_h;
