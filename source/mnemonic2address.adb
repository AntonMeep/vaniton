with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;

with Addresses;    use Addresses;
with Cryptography; use Cryptography;
with Wallets;      use Wallets;
with Mnemonics;    use Mnemonics;
with Words;        use Words.Bounded_Words;

procedure Mnemonic2Address is
   Config : Command_Line_Configuration;
begin
   Set_Usage
     (Config, Usage => "[switches] [mnemonic phrase]",
      Help          =>
        "Tool to compute wallet address for the supplied mnemonic phrase." &
        ASCII.LF &
        "Part of vaniton project: <https://github.com/AntonMeep/vaniton/>" &
        ASCII.LF);

   Getopt (Config);

   declare
      Phrase : Mnemonic (1 .. 24);
      Keys   : Key_Pair;
   begin
      for I in Phrase'Range loop
         Phrase (I) := To_Bounded_String (Get_Argument);
      end loop;

      if not Is_Valid (Phrase) then
         raise Program_Error with "Supplied mnemonic phrase is not valid";
      end if;

      Keys := To_Key_Pair (Phrase);

      for Kind in Wallet_Kind'Range loop
         for Test_Only in Boolean'Range loop
            for Bounceable in Boolean'Range loop
               Put (Kind'Image & " | ");
               if Test_Only then
                  Put ("T | ");
               else
                  Put ("M | ");
               end if;

               if Bounceable then
                  Put ("B | ");
               else
                  Put ("N | ");
               end if;

               Put_Line
                 (To_String
                    (Get_Wallet_Address
                       (Public_Key => Keys.Public_Key, Kind => Kind,
                        Test_Only  => Test_Only, Bounceable => Bounceable)));
            end loop;
         end loop;
      end loop;
   end;
exception
   when Exit_From_Command_Line =>
      GNAT.OS_Lib.OS_Exit (0); -- All chill
   when Error : others         =>
      Put (Exception_Information (Error));
      GNAT.OS_Lib.OS_Exit (-1);
end Mnemonic2Address;
