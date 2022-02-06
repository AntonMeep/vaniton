pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package hmac_sha_c is

   function hmac_sha512
     (key : access Unsigned_8; key_len : size_t; message : access Unsigned_8;
      message_length : size_t; c_out : access Unsigned_8; nout : size_t)
      return Extensions.bool  -- .\source\hmac_sha.c:6
   with
      Import        => True,
      Convention    => C,
      External_Name => "hmac_sha512";

end hmac_sha_c;
