with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;

with Workers; use Workers;

procedure Vaniton is
   Config            : Command_Line_Configuration;
   Number_Of_Workers : aliased Integer;

   type Worker_Array_Type is array (Natural range <>) of Worker;
   Worker_Array : access Worker_Array_Type;
   The_Matcher  : Matcher;
begin
   Define_Switch
     (Config, Number_Of_Workers'Access, "-w:", "--workers=",
      "Number of working threads running in parallel", Initial => 1);

   Getopt (Config);

   Worker_Array :=
     new Worker_Array_Type (0 .. Natural (Number_Of_Workers) - 1);
   for I in Worker_Array.all'Range loop
      Worker_Array.all (I).Start;
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
