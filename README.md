vaniton ![GitHub Workflow Status](https://img.shields.io/github/workflow/status/AntonMeep/vaniton/CI%20linux) ![GitHub](https://img.shields.io/github/license/AntonMeep/vaniton) ![Donation address](https://img.shields.io/badge/donate%20TON-EQAnTon5VVNKup8v0EUT0SvCKsRmEpotr__3eSpqYJTneIVht%20-blue)
=======

**vaniton** is a vanity wallet address generator for [The Open Network's](https://ton.org/) blockchain. Currently it supports all major versions of wallet smart contracts you could encounter (from SimpleR1 to V4R2).

> **Disclaimer**:
Always verify that mnemonic generated by this program corresponds to address by importing it into a wallet of your choice. As any other software this program may contain bugs.

# Help
```
Vanity address generator for The Open Network <https://ton.org/> blockchain wallets.
Generates 24-word secret phrases and computes wallet address for the selected wallet
 contract version (default: V3_R2).
Optionally, addresses are filtered out by matching against [pattern].
Project page: <https://github.com/AntonMeep/vaniton/>

Usage: vaniton.exe [switches] [pattern]

 -j, --threads=ARG    Number of working threads running in parallel
 -w, --wallet=ARG     Wallet version to use (default: V3_R2)
 -c, --case-sensitive Match case-sensitive (default: FALSE)
 -l, --log=ARG        Log program output to file (default: '')
 -wsimpler1           Equivalent to --wallet=Simple_R1
 -wsimpler2           Equivalent to --wallet=Simple_R2
 -wsimpler3           Equivalent to --wallet=Simple_R3
 -wsimple             Equivalent to --wallet=Simple_R3
 -wv2r1               Equivalent to --wallet=V2_R1
 -wv2r2               Equivalent to --wallet=V2_R2
 -wv2                 Equivalent to --wallet=V2_R2
 -wv3r1               Equivalent to --wallet=V3_R1
 -wv3r2               Equivalent to --wallet=V3_R2
 -wv3                 Equivalent to --wallet=V3_R2
 -wv4r1               Equivalent to --wallet=V4_R1
 -wv4r2               Equivalent to --wallet=V4_R2
 -wv4                 Equivalent to --wallet=V4_R2
```

# Pattern format
Currently, program implements only a simple subset of regular expression patterns, here's a quick summary:

| Expression | Description |
|------------|-------------|
| a|b        | Matches 'a' or 'b' |
| ab         | Matches 'ab' |
| a*         | Matches zero or more 'a's |
| a+         | Matches one or more 'a's |
| a?         | Matches one 'a' or nothing |
| [ab]       | Matches 'a' or 'b' |
| [^ab]      | Matches anything but 'a' or 'b' |
| [a-c]      | Matches 'a' or 'b' or 'c' |
| .          | Matches anything |
| (abc)      | Grouping |

Note, that patterns in **vaniton** match entire address string, and by default are case-insensitive (can be changed with `-c` switch). If you get stuck, it might be worth it to redirect output into *grep*, *awk*, or some other program of your liking to filter out unwanted addresses.

Here are some of the examples to get you started:
```
$ vaniton .*abc.*           # Match any address that has "abc" anywhere in it
$ vaniton .*t[o0]n.*        # Match any address that has "ton" or "t0n" anywhere in it
$ vaniton ..al[i1]z[e3]r.* # Since first two characters are always "EQ" and third character is [ABCD], this would match addresses that start with "EQalizer", "EQal1z3r", "EQaliz3r", or "EQal1z3r"
```

# Safety
This program utilizes same algorithms used by all TON wallets and relies on well-tested cryptography libraries such as OpenSSL and SPARKNaCl. While corners were certainly cut to improve overall performance, safety was not compromised.

Furthermore, **vaniton** does not have any online capabilities, dynamically loaded code (apart from abovementioned OpenSSL on certain systems), nor a large number of obscure dependencies.

# Are you cracking passwords? Can I accidentally get access to someone else's funds?
Impossible, this will never happen. Although mathematics forces me to say that this is *possible*, it is so unimaginably improbable that calling it impossible is a pretty good approximation.

See, secret phrase consists of a 24-word mnemonic phrase where each word can be one of 2048 words of a special wordlist. This gives us 2048^24 or about 2.9\*10^79 possible combinations. It is rather difficult to wrap one's head around a number this big, but just for the sake of comparison, Solar System, the entirety of it, consists of about 1.2\*10^56 atoms. Yep, it just ain't gonna happen

# Speed
**vaniton** is extremely slow compared to similar vanity address generators made for different blockchains. This all comes down to key derivation functions used by TON wallets, which increases time required to calculate a private key from supplied mnemonic phrase.
This is done to greatly decrease efficiency of brute-force attacks where an attacker would try to blindly go over a large number of private keys. Unfortunately, this is pretty much what searching for a vanity address is like.

Tips on improving performance:
- Use as many worker threads as possible
- Limit number of characters you're searching for
- Search for characters of both upper and lower case
- 1337. Try t0 s34rch f0r numb3rs t00

Here's an example table that shows you how much time it takes to match N characters. Albeit time to match a certain set of characters is largely dependent on your luck, and these measures were made on a fairly slow virtual machine, this can give you a good idea on what to expect.
You can used supplied `benchmark` tool to measure address generation speed of your machine.

| N | Time (s) | How much longer? |
|---|----------|------------------|
| 1 |      0.1 |                - |
| 2 |      3.9 |              39x |
| 3 |    595.6 |             153x |
| 4 |  17441.6 |              29x |


# Wallet contract versions
Due to the nature of TON blockchain, users' wallets *are* smart contracts. Over time, different versions of wallet smart contracts were used in TON. It is important to specify which version of a wallet you want to be generating addresses for. Here's a quick overview of different versions:
- Do not use Simple or V2 wallet versions, these are largely obsolete
- V3R2 is the default version of wallets created by all major wallet software. This is also the default for vaniton and a good place to start
- V4R2 is, at the time of writing, the latest and greatest version that introduces plug-in functionality. Choose this if you want to have a future-proof wallet

>It is important to note that basic functionality of all wallet versions is the same - you don't have to update whenever a new version comes out

# Importing the mnemonic phrase
After **vaniton** created a nice address for you, you can import the mnemonic phrase it generated into a wallet software of your liking. If version of the wallet contract is different than the software's default (i.e. not V3R2), then your wallet software will show you a completely different address. Fear not, once you deposit some funds to the address generated by **vaniton**, your wallet software should recognize that and switch to use it (re-importing your mnemonic phrase might be necessary).

Be aware, that some wallet software may implement automatic wallet smart contract upgrades by sending funds from an older version to a newer version, which can completely ruin the purpose of using **vaniton** to generate the address.

# Donations
Donations will help me to work on improvement of this project!
Feel free to drop a few TON cents to the following address:
> EQAnTon5VVNKup8v0EUT0SvCKsRmEpotr_3eSpqYJTneIVht

Yes, this address was in fact generated by **vaniton** over a course of a few days :)
