with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;

with Contracts; use Contracts;
with Workers;   use Workers;

procedure Vaniton is
   Config             : Command_Line_Configuration;
   Number_Of_Workers  : aliased Integer;
   Wallet_Kind_String : aliased String_Access := new String'("V3R2");
   Wallet_Kind        : Wallet_Contract;

   type Worker_Array_Type is array (Natural range <>) of Worker;
   Worker_Array : access Worker_Array_Type;
   The_Matcher  : Matcher;
begin
   Define_Switch
     (Config, Number_Of_Workers'Access, "-t:", "--threads=",
      "Number of working threads running in parallel", Initial => 1);
   Define_Switch
     (Config, Wallet_Kind_String'Access, "-w:", "--wallet=",
      "Wallet version to use (default: V3R2)");

   Getopt (Config);

   Wallet_Kind_String.all := To_Upper (Wallet_Kind_String.all);
   if Wallet_Kind_String.all = "simpler1" then
      Wallet_Kind := Wallet_Contract_Simple_R1;
   elsif Wallet_Kind_String.all = "simpler2" then
      Wallet_Kind := Wallet_Contract_Simple_R2;
   elsif Wallet_Kind_String.all = "simpler3"
     or else Wallet_Kind_String.all = "simple"
   then
      Wallet_Kind := Wallet_Contract_Simple_R3;
   elsif Wallet_Kind_String.all = "v2r1" then
      Wallet_Kind := Wallet_Contract_V2_R1;
   elsif Wallet_Kind_String.all = "v2r2" or else Wallet_Kind_String.all = "v2"
   then
      Wallet_Kind := Wallet_Contract_V2_R2;
   elsif Wallet_Kind_String.all = "v3r1" then
      Wallet_Kind := Wallet_Contract_V3_R1;
   elsif Wallet_Kind_String.all = "v3r2" or else Wallet_Kind_String.all = "v3"
   then
      Wallet_Kind := Wallet_Contract_V3_R2;
   elsif Wallet_Kind_String.all = "v4r1" then
      Wallet_Kind := Wallet_Contract_V4_R1;
   elsif Wallet_Kind_String.all = "v4r2" or else Wallet_Kind_String.all = "v4"
   then
      Wallet_Kind := Wallet_Contract_V4_R2;
   else
      raise Program_Error with "Unknown wallet kind specified";
   end if;

   Worker_Array :=
     new Worker_Array_Type (0 .. Natural (Number_Of_Workers) - 1);
   for I in Worker_Array.all'Range loop
      Worker_Array.all (I).Start (Wallet_Kind);
   end loop;
   The_Matcher.Start;

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
