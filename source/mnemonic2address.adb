with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;
with GNAT.OS_Lib;

with Addresses;    use Addresses;
with Cryptography; use Cryptography;
with Wallets;      use Wallets;
with Mnemonics;    use Mnemonics;
with Words;        use Words.Bounded_Words;

procedure Mnemonic2Address is
   Config             : Command_Line_Configuration;
   Wallet_Kind_String : aliased String_Access := new String'("");
begin
   Set_Usage
     (Config, Usage => "[switches] [mnemonic phrase]",
      Help          =>
        "Tool to compute wallet address for the supplied mnemonic phrase." &
        ASCII.LF &
        "Part of vaniton project: <https://github.com/AntonMeep/vaniton/>" &
        ASCII.LF);
   Define_Switch
     (Config, Wallet_Kind_String'Access, "-w:", "--wallet=",
      "Wallet version to use. If empty, prints addresses for all versions" &
      " (default: " & Wallet_Kind_String.all & ")");

   Define_Alias (Config, "-wsimpler1", "--wallet=Simple_R1");
   Define_Alias (Config, "-wsimpler2", "--wallet=Simple_R2");
   Define_Alias (Config, "-wsimpler3", "--wallet=Simple_R3");
   Define_Alias (Config, "-wsimple", "--wallet=Simple_R3");
   Define_Alias (Config, "-wv2r1", "--wallet=V2_R1");
   Define_Alias (Config, "-wv2r2", "--wallet=V2_R2");
   Define_Alias (Config, "-wv2", "--wallet=V2_R2");
   Define_Alias (Config, "-wv3r1", "--wallet=V3_R1");
   Define_Alias (Config, "-wv3r2", "--wallet=V3_R2");
   Define_Alias (Config, "-wv3", "--wallet=V3_R2");
   Define_Alias (Config, "-wv4r1", "--wallet=V4_R1");
   Define_Alias (Config, "-wv4r2", "--wallet=V4_R2");
   Define_Alias (Config, "-wv4", "--wallet=V4_R2");

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

      if Wallet_Kind_String.all'Length /= 0 then
         Put_Line
           (To_String
              (Get_Wallet_Address
                 (Public_Key => Keys.Public_Key,
                  Kind       => Wallet_Kind'Value (Wallet_Kind_String.all))));
      else
         for Kind in Wallet_Kind'Range loop
            Put_Line
              (Kind'Image & ": " &
               To_String
                 (Get_Wallet_Address
                    (Public_Key => Keys.Public_Key, Kind => Kind)));
         end loop;
      end if;
   end;
exception
   when Exit_From_Command_Line =>
      GNAT.OS_Lib.OS_Exit (0); -- All chill
   when Error : others =>
      Put (Exception_Information (Error));
      GNAT.OS_Lib.OS_Exit (-1);
end Mnemonic2Address;
