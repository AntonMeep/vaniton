with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;

with Wallets;
with Workers; use Workers;

procedure Vaniton is
   Config             : Command_Line_Configuration;
   Number_Of_Workers  : aliased Integer;
   Wallet_Kind_String : aliased String_Access := new String'("V3_R2");
   Wallet_Kind        : Wallets.Wallet_Kind;
   Pattern            : aliased String_Access := new String'("");
   Case_Sensitive     : aliased Boolean       := False;

   type Worker_Array_Type is array (Natural range <>) of Worker;
   Worker_Array : access Worker_Array_Type;
   The_Matcher  : Matcher;
begin
   Define_Switch
     (Config, Number_Of_Workers'Access, "-t:", "--threads=",
      "Number of working threads running in parallel", Initial => 1);
   Define_Switch
     (Config, Wallet_Kind_String'Access, "-w:", "--wallet=",
      "Wallet version to use (default: V3_R2)");
   Define_Switch
     (Config, Pattern'Access, "-p:", "--pattern=",
      "Pattern to match against (default: '' = match all)");
   Define_Switch
     (Config, Case_Sensitive'Access, "-c", "--case-sensitive",
      "Match case-sensitive (default: false)");

   Getopt (Config);

   Wallet_Kind := Wallets.Wallet_Kind'Value (Wallet_Kind_String.all);

   Worker_Array :=
     new Worker_Array_Type (0 .. Natural (Number_Of_Workers) - 1);
   for I in Worker_Array.all'Range loop
      Worker_Array.all (I).Start (Wallet_Kind);
   end loop;
   The_Matcher.Start (Pattern.all, Case_Sensitive);

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
end Vaniton;
