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
              From_BoC
                ("B5EE9C72410101010044000084FF0020DDA4F260810200D71820D70B1FED44D0D31FD3FFD15112BAF2A122F901541044F910F2A2F80001D31F3120D74A96D307D402FB00DED1A4C8CB1FCBFFC9ED5441FDF089");
         when Wallet_Contract_Simple_R2 =>
            Result.Code :=
              From_BoC
                ("B5EE9C724101010100530000A2FF0020DD2082014C97BA9730ED44D0D70B1FE0A4F260810200D71820D70B1FED44D0D31FD3FFD15112BAF2A122F901541044F910F2A2F80001D31F3120D74A96D307D402FB00DED1A4C8CB1FCBFFC9ED54D0E2786F");
         when Wallet_Contract_Simple_R3 =>
            Result.Code :=
              From_BoC
                ("B5EE9C7241010101005F0000BAFF0020DD2082014C97BA218201339CBAB19C71B0ED44D0D31FD70BFFE304E0A4F260810200D71820D70B1FED44D0D31FD3FFD15112BAF2A122F901541044F910F2A2F80001D31F3120D74A96D307D402FB00DED1A4C8CB1FCBFFC9ED54B5B86E42");
         when Wallet_Contract_V2_R1 =>
            Result.Code :=
              From_BoC
                ("B5EE9C724101010100570000AAFF0020DD2082014C97BA9730ED44D0D70B1FE0A4F2608308D71820D31FD31F01F823BBF263ED44D0D31FD3FFD15131BAF2A103F901541042F910F2A2F800029320D74A96D307D402FB00E8D1A4C8CB1FCBFFC9ED54A1370BB6");
         when Wallet_Contract_V2_R2 =>
            Result.Code :=
              From_BoC
                ("B5EE9C724101010100630000C2FF0020DD2082014C97BA218201339CBAB19C71B0ED44D0D31FD70BFFE304E0A4F2608308D71820D31FD31F01F823BBF263ED44D0D31FD3FFD15131BAF2A103F901541042F910F2A2F800029320D74A96D307D402FB00E8D1A4C8CB1FCBFFC9ED54044CD7A1");
         when Wallet_Contract_V3_R1 =>
            Result.Code :=
              From_BoC
                ("B5EE9C724101010100620000C0FF0020DD2082014C97BA9730ED44D0D70B1FE0A4F2608308D71820D31FD31FD31FF82313BBF263ED44D0D31FD31FD3FFD15132BAF2A15144BAF2A204F901541055F910F2A3F8009320D74A96D307D402FB00E8D101A4C8CB1FCB1FCBFFC9ED543FBE6EE0");
         when Wallet_Contract_V3_R2 =>
            Result.Code :=
              From_BoC
                ("B5EE9C724101010100710000DEFF0020DD2082014C97BA218201339CBAB19F71B0ED44D0D31FD31F31D70BFFE304E0A4F2608308D71820D31FD31FD31FF82313BBF263ED44D0D31FD31FD3FFD15132BAF2A15144BAF2A204F901541055F910F2A3F8009320D74A96D307D402FB00E8D101A4C8CB1FCB1FCBFFC9ED5410BD6DAD");
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
