with Types; use Types;

package Base64 is
   Base64_Error : exception;

   function From_Base64 (Input : String) return Byte_Array;
   function To_Base64 (Input : Byte_Array) return String;
   function To_Base64Url (Input : Byte_Array) return String;
end Base64;
