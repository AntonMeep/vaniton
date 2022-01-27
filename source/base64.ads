with Types; use Types;

package Base64 is
   function From_Base64 (Input : String) return Byte_Array;
   function To_Base64 (Input : Byte_Array) return String;
end Base64;
