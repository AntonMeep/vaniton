with Ada.Containers;
with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

with Wallets;
with Mnemonics; use Mnemonics;

package Workers is
   subtype Mnemonic24 is Mnemonic (1 .. 24);

   type Work_Unit is record
      Address : String (1 .. 48);
      Phrase  : Mnemonic24;
   end record;

   package Work_Queue_Interfaces is new Ada.Containers
     .Synchronized_Queue_Interfaces
     (Element_Type => Work_Unit);

   package Work_Queues is new Ada.Containers.Bounded_Synchronized_Queues
     (Queue_Interfaces => Work_Queue_Interfaces, Default_Capacity => 16#FFFF#);

   Work_Queue : Work_Queues.Queue;

   protected Control is
      procedure Signal_Stop;
      function Stop return Boolean;
   private
      Flag_Stop : Boolean := False;
   end Control;

   procedure Stop;

   task type Worker is
      entry Start
        (Kind    : Wallets.Wallet_Kind; 
         Test_Only : Boolean;
         Bounceable : Boolean;
         Pattern : String;
         Case_Sensitive : Boolean);
   end Worker;

   task type Writer is
      entry Start (File_Name : String := "");
   end Writer;
end Workers;
