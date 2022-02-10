with Ada.Calendar;   use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.Formatted_String; use GNAT.Formatted_String;
with GNAT.OS_Lib;
with GNAT.Regexp;           use GNAT.Regexp;

with Wallets;
with Workers; use Workers;

procedure Benchmark is
   Config            : Command_Line_Configuration;
   Number_Of_Workers : aliased Integer;
   Iterations        : aliased Integer;

   type Worker_Array_Type is array (Natural range <>) of Worker;
   type Worker_Array_Access is access all Worker_Array_Type;
   Worker_Array : Worker_Array_Access := null;
begin
   Set_Usage
     (Config, Usage => "[switches]",
      Help          =>
        "Benchmark for vaniton, the vanity generator for TON" & ASCII.LF &
        "Part of project vaniton: <https://github.com/AntonMeep/vaniton/>" &
        ASCII.LF);

   Define_Switch
     (Config, Number_Of_Workers'Access, "-j:", "--threads=",
      "Number of working threads running in parallel", Initial => 1);
   Define_Switch
     (Config, Iterations'Access, "-n:", "--iterations=",
      "Number of iterations", Initial => 100);

   Getopt (Config);

   Worker_Array :=
     new Worker_Array_Type (0 .. Natural (Number_Of_Workers) - 1);
   for I in Worker_Array.all'Range loop
      Worker_Array.all (I).Start (Wallets.V3_R2, "", False);
   end loop;

   declare
      Start_Time : constant Time := Clock;
      Index      : Natural       := 0;
      Temporary  : Work_Unit;
      Taken      : Duration;
   begin
      Put_Line ("[benchmark] Measuring address generation speed...");
      loop
         exit when Index = Iterations;
         Work_Queue.Dequeue (Temporary);
         Index := Index + 1;
      end loop;

      Taken := Clock - Start_Time;
      Put_Line
        (-
         (+"[benchmark] It took %fs; %f addresses/sec; %f addressses/(sec*thread)" &
          Taken & ((Iterations * 1.0) / Taken) &
          ((Iterations * 1.0) / Taken / Number_Of_Workers)));
   end;

   declare
      type Regexp_Array is array (Positive range <>) of Regexp;

      Expressions : constant Regexp_Array (1 .. 5) :=
        (Compile (".*a.*"), Compile (".*ab.*"), Compile (".*abc.*"),
         Compile (".*abcd.*"), Compile (".*abcde.*"));

      Start_Time         : Time     := Clock;
      Current            : Work_Unit;
      Currently_Matching : Positive := 1;
      Index              : Integer  := 1;
   begin
      Put_Line ("[benchmark] Measuring time it takes to match N characters");

      loop
         Work_Queue.Dequeue (Current);

         if Match (Current.Address, Expressions (Currently_Matching)) then
            declare
               Taken : constant Duration := Clock - Start_Time;
            begin
               Put_Line
                 (-
                  (+("[benchmark] Matched %d character(s) in %fs after %d " &
                    "attempts; %f addresses/sec; %f addresses/(sec*thread).") &
                   Currently_Matching & Taken & Index &
                   ((Index * 1.0) / Taken) &
                   ((Index * 1.0) / Taken / Number_Of_Workers)));

               Start_Time         := Clock;
               Currently_Matching := Currently_Matching + 1;
            end;
         end if;

         Index := Index + 1;
         exit when Currently_Matching > 5;
      end loop;
   end;

   Control.Signal_Stop;
exception
   when Exit_From_Command_Line =>
      GNAT.OS_Lib.OS_Exit (0); -- All chill
   when Error : others =>
      Put (Exception_Information (Error));
      GNAT.OS_Lib.OS_Exit (-1);
end Benchmark;
