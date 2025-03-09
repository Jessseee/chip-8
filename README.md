<h1 align="center">
<img src="logo.png" alt="CHIP-8 logo">
<br>
Rust CHIP-8 Interpreter
</h1>
<p align="center">
A simple CHIP-8 interpreter written in Rust.
</p>

The "Hello World!" of emulator development. [CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) is an interpreter originally developed for the [RCA COSMAC VIP](https://en.wikipedia.org/wiki/COSMAC_VIP) to write games. It is a great starting point for emulator development as you will not have to concern yourself with hardware details and can focus on parsing the simple instruction set. There are many guides, article and ROMs available for CHIP-8 which makes it easy to get started. In addition to this there is the [Octo](https://github.com/JohnEarnest/Octo) high level assembler for CHIP-8 by John Earnest, which also makes it easy to write your own CHIP-8 games!

This interpreter currently supports the original CHIP-8 instruction set and the SUPER-CHIP extension, including similar platforms with quirks. To choose a platform and its respective quirks the interpreter uses the [chip-8 database](https://github.com/chip-8/chip-8-database/) published by the CHIP-8 organisation on GitHub. To verify whether my implementation works I used Tim Franssen's [CHIP-8 test suite](https://github.com/Timendus/chip8-test-suite) and also wrote automated tests which use his quirks program.

My implementation of this interpreter was written as en exercise to improve my familiarity with the Rust programming language. In this process I used the help of this guide by [Tobias V. Langhoff](https://tobiasvl.github.io/blog/write-a-chip-8-emulator), detailed articles by [Laurence Scotford](https://www.laurencescotford.net/2020/07/25/chip-8-on-the-cosmac-vip-index/), and the advice of the people in the [Emulator Development Discord](https://discord.gg/dkmJAes).
