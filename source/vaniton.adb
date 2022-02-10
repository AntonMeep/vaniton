with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;
with GNAT.OS_Lib;

with Wallets;
with Workers; use Workers;

procedure Vaniton is
   Config             : Command_Line_Configuration;
   Number_Of_Workers  : aliased Integer;
   Wallet_Kind_String : aliased String_Access := new String'("V3_R2");
   Case_Sensitive     : aliased Boolean       := False;
   Log_File           : aliased String_Access := new String'("");

   type Worker_Array_Type is array (Natural range <>) of Worker;
   Worker_Array  : access Worker_Array_Type := null;
   The_Writer    : Writer;
   The_Benchmark : Benchmarker;
begin
   --!pp off
   Set_Usage
     (Config, Usage => "[switches] [pattern]",
      Help          =>
        "Vanity address generator for The Open Network <https://ton.org/>" &
        " blockchain wallets." & ASCII.LF &
        "Generates 24-word secret phrases and computes wallet address for" &
        " the selected wallet" & ASCII.LF &
        " contract version (default: " & Wallet_Kind_String.all & ")." &
        ASCII.LF &
        "Optionally, addresses are filtered out by matching against" &
        " [pattern]." & ASCII.LF &
        "Project page: <https://github.com/AntonMeep/vaniton/>" & ASCII.LF);
   --!pp on

   Define_Switch
     (Config, Number_Of_Workers'Access, "-j:", "--threads=",
      "Number of working threads running in parallel", Initial => 1);
   Define_Switch
     (Config, Wallet_Kind_String'Access, "-w:", "--wallet=",
      "Wallet version to use (default: " & Wallet_Kind_String.all & ")");
   Define_Switch
     (Config, Case_Sensitive'Access, "-c", "--case-sensitive",
      "Match case-sensitive (default: " & Case_Sensitive'Img & ")");
   Define_Switch
     (Config, Log_File'Access, "-l:", "--log=",
      "Log program output to file (default: '" & Log_File.all & "')");

   Define_Alias (Config, "-wsimpler1", "--wallet=SimpleR1");
   Define_Alias (Config, "-wsimpler2", "--wallet=SimpleR2");
   Define_Alias (Config, "-wsimpler3", "--wallet=SimpleR3");
   Define_Alias (Config, "-wsimple", "--wallet=SimpleR3");
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
      Pattern     : constant String              := Get_Argument;
      Wallet_Kind : constant Wallets.Wallet_Kind :=
        Wallets.Wallet_Kind'Value (Wallet_Kind_String.all);
   begin
      Worker_Array :=
        new Worker_Array_Type (0 .. Natural (Number_Of_Workers) - 1);
      for I in Worker_Array.all'Range loop
         Worker_Array.all (I).Start (Wallet_Kind, Pattern, Case_Sensitive);
      end loop;
   end;

   The_Writer.Start (Log_File.all);

   declare
      Key       : Character;
      Available : Boolean;
   begin
      while True loop
         Ada.Text_IO.Get_Immediate (Key, Available);
         exit when Available;
      end loop;
   end;

   Control.Signal_Stop;
exception
   when Exit_From_Command_Line =>
      GNAT.OS_Lib.OS_Exit (0); -- All chill
   when Error : others =>
      Put (Exception_Information (Error));
      GNAT.OS_Lib.OS_Exit (-1);
end Vaniton;
