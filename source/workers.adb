pragma Ada_2012;

with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;
with GNAT.Regexp;

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
      Current : Work_Unit;
      Kind    : Wallets.Wallet_Kind;
   begin
      accept Start (Wallet_Kind : Wallets.Wallet_Kind) do
         Kind := Wallet_Kind;
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

         Work_Queue.Enqueue (Current);
      end loop;
   end Worker;

   task body Matcher is
      use Ada.Text_IO;
      use GNAT.Regexp;

      Current    : Work_Unit;
      Expression : Regexp;
   begin
      accept Start (Pattern : String; Case_Sensitive : Boolean) do
         Expression := Compile (Pattern, False, Case_Sensitive);

      end Start;

      while (not Control.Stop) or else (Work_Queue.Current_Use /= 0) loop
         Work_Queue.Dequeue (Current);

         if Match (Current.Address, Expression) then
            Put_Line (Current.Address & "|" & To_String (Current.Phrase));
         end if;
      end loop;
   end Matcher;

end Workers;
