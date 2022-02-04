pragma Ada_2012;

with Ada.Text_IO;

with Addresses;
with Contracts;
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
      Current : Work_Unit;
   begin
      accept Start;

      while not Control.Stop loop
         Current.Phrase := Generate;
         declare
            use Contracts;
            use Cryptography;

            KP : constant Key_Pair := To_Key_Pair (Current.Phrase);
         begin
            Current.Address :=
              Addresses.To_String
                (Get_Address
                   (Create
                      (Public_Key => KP.Public_Key,
                       Kind       => Wallet_Contract_V3_R2)));
         end;

         Work_Queue.Enqueue (Current);
      end loop;
   end Worker;

   task body Matcher is
      use Ada.Text_IO;

      Current : Work_Unit;
   begin
      accept Start;

      while not Control.Stop loop
         Work_Queue.Dequeue (Current);
         Put_Line (Current.Address & "|" & To_String (Current.Phrase));
      end loop;
   end Matcher;

end Workers;
