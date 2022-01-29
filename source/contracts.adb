pragma Ada_2012;
package body Contracts is
   procedure Create
     (This      : in out WalletV3R1_Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0)
   is
   begin
      This.Code :=
        From_Bits
          ("FF0020DD2082014C97BA9730ED44D0D70B1FE0A4F2608308D71820D31FD31FD31FF82313BBF263ED44D0D31FD31FD3FFD15132BAF2A15144BAF2A204F901541055F910F2A3F8009320D74A96D307D402FB00E8D101A4C8CB1FCB1FCBFFC9ED54");
      This.Data := Empty_Cell;
      Write (This.Data, Unsigned_32 (0));
      Write (This.Data, Unsigned_32 (698_983_191) + Unsigned_32 (Workchain));
      Write (This.Data, Public_Key);
   end Create;

   procedure Create
     (This      : in out WalletV3R2_Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      raise Program_Error with "Unimplemented procedure Create";
   end Create;

   procedure Create
     (This      : in out WalletV4_Contract; Public_Key : Byte_Array;
      Workchain :        Integer_8 := 0)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      raise Program_Error with "Unimplemented procedure Create";
   end Create;

   function Get_Address (This : in out Contract) return Address is
      State_Init       : Cell;
      State_Init_Array : Bit_Array (1 .. 5) :=
        (False, -- split depth
         False, -- tick tock
         True, -- code
         True, -- data
         False -- library
      );
   begin
      Write (State_Init, State_Init_Array);
      Append_Reference (State_Init, This.Code'Access);
      Append_Reference (State_Init, This.Data'Access);

      declare
         State_Init_Hash : Byte_Array := Hash (State_Init);
      begin
         return Create (False, False, This.Workchain, State_Init_Hash);
      end;
   end Get_Address;
end Contracts;
