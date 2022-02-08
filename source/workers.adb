pragma Ada_2012;

with Ada.Containers;           use Ada.Containers;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
with Ada.Text_IO;
with GNAT.Regexp;
with GNAT.Formatted_String;    use GNAT.Formatted_String;

with Addresses;
with Cryptography;

package body Workers is
   protected body Control is
      procedure Signal_Stop is
      begin
         Flag_Stop := True;
      end Signal_Stop;

      function Stop return Boolean is (Flag_Stop);
   end Control;

   task body Worker is
      use GNAT.Regexp;

      Current    : Work_Unit;
      Kind       : Wallets.Wallet_Kind;
      Expression : Regexp;
   begin
      accept Start
        (Wallet_Kind    : Wallets.Wallet_Kind; Pattern : String;
         Case_Sensitive : Boolean)
      do
         Kind       := Wallet_Kind;
         Expression := Compile (Pattern, False, Case_Sensitive);
      end Start;

      while not Control.Stop loop
         Current.Phrase := Generate;
         declare
            use Cryptography;

            KP : constant Key_Pair := To_Key_Pair (Current.Phrase);
         begin
            Current.Address :=
              Addresses.To_String
                (Wallets.Get_Wallet_Address
                   (Public_Key => KP.Public_Key, Kind => Kind));
         end;

         if Match (Current.Address, Expression) then
            Work_Queue.Enqueue (Current);
         end if;
      end loop;
   end Worker;

   task body Writer is
      use Ada.Text_IO;

      Current : Work_Unit;

      type File_Access_Array is array (Positive range <>) of File_Access;
      Outputs_Array : access File_Access_Array;

      Output_File : aliased File_Type;
   begin
      select
         accept Start (File_Name : String := "") do
            if File_Name = "" then
               Outputs_Array         := new File_Access_Array (1 .. 1);
               Outputs_Array.all (1) := Standard_Output;
            else
               Create (Output_File, Append_File, File_Name);

               Outputs_Array         := new File_Access_Array (1 .. 2);
               Outputs_Array.all (1) := Standard_Output;
               Outputs_Array.all (2) := Output_File'Unchecked_Access;
            end if;
         end Start;
      or
         terminate;
      end select;

      while (not Control.Stop) or else (Work_Queue.Current_Use /= 0) loop
         Work_Queue.Dequeue (Current);

         for File of Outputs_Array.all loop
            Put_Line
              (File.all, Current.Address & "|" & To_String (Current.Phrase));
         end loop;
      end loop;
   end Writer;

   task body Benchmarker is
      use Ada.Calendar;
      use Ada.Text_IO;
      use GNAT.Regexp;

      type Regexp_Array is array (Positive range <>) of Regexp;

      Expressions : constant Regexp_Array (1 .. 10) :=
        (Compile (".*a.*"), Compile (".*ab.*"), Compile (".*abc.*"),
         Compile (".*abcd.*"), Compile (".*abcde.*"), Compile (".*abcdef.*"),
         Compile (".*abcdefg.*"), Compile (".*abcdefgh.*"),
         Compile (".*abcdefghi.*"), Compile (".*abcdefghij.*"));
      Currently_Matching : Positive := 1;
      Start_Time         : Time;

      Current : Work_Unit;
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      Start_Time := Clock;

      while (not Control.Stop) or else (Work_Queue.Current_Use /= 0) loop
         Work_Queue.Dequeue (Current);

         if Match (Current.Address, Expressions (Currently_Matching)) then
            declare
               Taken : constant Duration := Clock - Start_Time;
            begin
               Put_Line
                 (-
                  (+"[benchmark] Matched %d characters in %ss." &
                   Currently_Matching & Duration'Image (Taken)));

               Start_Time         := Clock;
               Currently_Matching := Currently_Matching + 1;
            end;
         end if;
      end loop;
   end Benchmarker;
end Workers;
