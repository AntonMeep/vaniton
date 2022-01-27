pragma Ada_2012;
package body Base64 is
   function From_Base64 (Input : String) return Byte_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "From_Base64 unimplemented");
      return raise Program_Error with "Unimplemented function From_Base64";
   end From_Base64;

   function To_Base64 (Input : Byte_Array) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Base64 unimplemented");
      return raise Program_Error with "Unimplemented function To_Base64";
   end To_Base64;

end Base64;
