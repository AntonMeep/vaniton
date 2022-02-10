pragma Ada_2012;

with Ada.Containers;          use Ada.Containers;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Regexp;
with GNAT.Formatted_String;   use GNAT.Formatted_String;

with Interfaces; use Interfaces;

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
      Start_Time : Time;
      Index      : Unsigned_64 := 1;
   begin
      accept Start
        (Wallet_Kind    : Wallets.Wallet_Kind; Pattern : String;
         Case_Sensitive : Boolean)
      do
         Kind       := Wallet_Kind;
         Expression := Compile (Pattern, False, Case_Sensitive);
         Start_Time := Clock;
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

            if Match (Current.Address, Expression) then
               Work_Queue.Enqueue (Current);
            end if;
         end;

         if (Index and Unsigned_64 (16#FFF#)) = 0 then
            declare
               Taken : constant Duration := Clock - Start_Time;
            begin
               Put_Line
                 (Standard_Error,
                  -(+"[%s] Last %d took %fs (%f addresses/sec)" &
                   Image (Current_Task) & Integer (16#FFF#) & Taken &
                   ((16#FFF# * 1.0) / Taken)));
            end;
         end if;

         Index := Index + 1;
      end loop;
   end Worker;

   task body Writer is
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
end Workers;
