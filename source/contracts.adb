pragma Ada_2012;
package body Contracts is
   function Create
     (Public_Key : Byte_Array; Workchain : Integer_8 := 0;
      Kind       : Wallet_Contract := Wallet_Contract_V3_R2) return Contract
   is
      Result : Contract;
   begin
      Result.Kind      := Kind;
      Result.Workchain := Workchain;

      case Result.Kind is
         when Wallet_Contract_Simple_R1 =>
            Result.Code :=
              From_Bits
                ("ff0020dda4f260810200d71820d70b1fed44d0d31fd3ffd15112baf2a122f901541044f910f2a2f80001d31f3120d74a96d307d402fb00ded1a4c8cb1fcbffc9ed54");
         when Wallet_Contract_Simple_R2 =>
            Result.Code :=
              From_Bits
                ("ff0020dd2082014c97ba9730ed44d0d70b1fe0a4f260810200d71820d70b1fed44d0d31fd3ffd15112baf2a122f901541044f910f2a2f80001d31f3120d74a96d307d402fb00ded1a4c8cb1fcbffc9ed54");
         when Wallet_Contract_Simple_R3 =>
            Result.Code :=
              From_Bits
                ("ff0020dd2082014c97ba218201339cbab19c71b0ed44d0d31fd70bffe304e0a4f260810200d71820d70b1fed44d0d31fd3ffd15112baf2a122f901541044f910f2a2f80001d31f3120d74a96d307d402fb00ded1a4c8cb1fcbffc9ed54");
         when Wallet_Contract_V2_R1 =>
            Result.Code :=
              From_Bits
                ("ff0020dd2082014c97ba9730ed44d0d70b1fe0a4f2608308d71820d31fd31f01f823bbf263ed44d0d31fd3ffd15131baf2a103f901541042f910f2a2f800029320d74a96d307d402fb00e8d1a4c8cb1fcbffc9ed54");
         when Wallet_Contract_V2_R2 =>
            Result.Code :=
              From_Bits
                ("ff0020dd2082014c97ba218201339cbab19c71b0ed44d0d31fd70bffe304e0a4f2608308d71820d31fd31f01f823bbf263ed44d0d31fd3ffd15131baf2a103f901541042f910f2a2f800029320d74a96d307d402fb00e8d1a4c8cb1fcbffc9ed54");
         when Wallet_Contract_V3_R1 =>
            Result.Code :=
              From_Bits
                ("ff0020dd2082014c97ba9730ed44d0d70b1fe0a4f2608308d71820d31fd31fd31ff82313bbf263ed44d0d31fd31fd3ffd15132baf2a15144baf2a204f901541055f910f2a3f8009320d74a96d307d402fb00e8d101a4c8cb1fcb1fcbffc9ed54");
         when Wallet_Contract_V3_R2 =>
            Result.Code :=
              From_Bits
                ("ff0020dd2082014c97ba218201339cbab19f71b0ed44d0d31fd31f31d70bffe304e0a4f2608308d71820d31fd31fd31ff82313bbf263ed44d0d31fd31fd3ffd15132baf2a15144baf2a204f901541055f910f2a3f8009320d74a96d307d402fb00e8d101a4c8cb1fcb1fcbffc9ed54");
         when Wallet_Contract_V4_R1 =>
            -- TODO - this requires reading from BoC. Fuck meee
            raise Program_Error;
         when Wallet_Contract_V4_R2 =>
            -- Same here
            raise Program_Error;
      end case;

      case Result.Kind is
         when Wallet_Contract_Simple_R1 | Wallet_Contract_Simple_R2 |
           Wallet_Contract_Simple_R3 | Wallet_Contract_V2_R1 |
           Wallet_Contract_V2_R2 =>
            Result.Data := Empty_Cell;
            Write (Result.Data, Unsigned_32 (0));
            Write (Result.Data, Public_Key);
         when Wallet_Contract_V3_R1 | Wallet_Contract_V3_R2 =>
            Result.Data := Empty_Cell;
            Write (Result.Data, Unsigned_32 (0));
            Write
              (Result.Data,
               Unsigned_32 (698_983_191 + Integer (Result.Workchain)));
            Write (Result.Data, Public_Key);
         when Wallet_Contract_V4_R1 =>
            -- TODO
            raise Program_Error;
         when Wallet_Contract_V4_R2 =>
            -- Same here
            raise Program_Error;
      end case;

      return Result;
   end Create;

   function Get_Address (This_Original : in Contract) return Address is
      This : Contract := This_Original;

      State_Init       : Cell               := Empty_Cell;
      State_Init_Array : Bit_Array (1 .. 5) :=
        (False, -- split depth
         False, -- tick tock
         True, -- code
         True, -- data
         False -- library
      );

   begin
      Write (State_Init, State_Init_Array);
      Append_Reference
        (State_Init, This.Code'Unchecked_Access); -- Live fast, use Unchecked_
      Append_Reference (State_Init, This.Data'Unchecked_Access);

      declare
         State_Init_Hash : Byte_Array := Hash (State_Init);
      begin
         return Create (False, True, This.Workchain, State_Init_Hash);
      end;
   end Get_Address;
end Contracts;
