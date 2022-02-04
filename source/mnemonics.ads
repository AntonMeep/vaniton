with Cryptography; use Cryptography;
with Words;        use Words;
with Types;        use Types;

package Mnemonics is
   type Mnemonic is array (Positive range <>) of Bounded_Word;

   function Generate
     (Words_Count : Positive := 24; Password : String := "";
      List        : Wordlist := English_Words) return Mnemonic;
   function From_String (Input : String) return Mnemonic;
   function To_String (This : Mnemonic) return String;
   function Is_Valid
     (This : Mnemonic; Password : String := "";
      List : Wordlist := English_Words) return Boolean;
   function To_Entropy
     (This : Mnemonic; Password : String := "") return Byte_Array;
   function To_Seed
     (This : Mnemonic; Password : String := "") return Byte_Array;
   function To_Key_Pair
     (This : Mnemonic; Password : String := "") return Key_Pair;
end Mnemonics;
