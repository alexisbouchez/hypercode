import type { Lesson } from "../../types";

export const bitManipulation: Lesson = {
  id: "bit-manipulation",
  title: "Bit Manipulation Patterns",
  chapterId: "bitwise",
  content: `## Common Bit Manipulation Patterns

These patterns appear constantly in systems programming -- from device drivers and OS kernels to compression algorithms and game engines. Mastering them makes you a more effective low-level programmer.

> Like Data's positronic brain, bit manipulation is about tweaking individual neural pathways one bit at a time -- precise, methodical, and deeply satisfying.

### Set, Clear, and Toggle Bits

These three operations are the building blocks of bit manipulation:

\`\`\`asm
// Set bit N (turn it ON):
MOV X1, #1
LSL X1, X1, #3       // mask = 1 << 3 = 0b1000
ORR X0, X0, X1       // Set bit 3

// Clear bit N (turn it OFF):
MOV X1, #1
LSL X1, X1, #3
MVN X1, X1           // mask = ~(1 << 3) = 0b...0111
AND X0, X0, X1       // Clear bit 3

// Toggle bit N (flip it):
MOV X1, #1
LSL X1, X1, #3
EOR X0, X0, X1       // Toggle bit 3
\`\`\`

### TST -- Test Bits

\`TST\` performs AND and sets flags, but discards the result. It is the most efficient way to test if specific bits are set:

\`\`\`asm
TST X0, #0x08        // Test bit 3: flags reflect X0 & 0x08
B.NE bit_is_set      // Branch if bit 3 is set (result non-zero)
\`\`\`

\`TST\` is equivalent to \`ANDS XZR, X0, #0x08\` -- it ANDs and sets flags into the zero register.

### Check if Power of Two

A number is a power of two if it has exactly one bit set. The trick: \`n & (n-1)\` clears the lowest set bit. If the result is zero, there was only one bit:

\`\`\`asm
// Is X0 a power of 2? (assumes X0 > 0)
SUB X1, X0, #1       // X1 = n - 1
TST X0, X1           // Flags reflect n & (n-1)
B.EQ is_power        // Zero means exactly one bit was set
\`\`\`

For example: \`8 (1000) & 7 (0111) = 0\`. But \`6 (0110) & 5 (0101) = 4\`.

### Counting Set Bits (Popcount)

Population count (popcount) counts how many bits are 1. The approach: test the lowest bit, add it to a counter, shift right, repeat until zero:

\`\`\`asm
MOV X1, #0           // count = 0
count_loop:
    CBZ X0, count_done
    AND X2, X0, #1   // test lowest bit
    ADD X1, X1, X2   // count += bit value (0 or 1)
    LSR X0, X0, #1   // shift right (discard lowest bit)
    B count_loop
count_done:
// X1 = number of set bits
\`\`\`

Popcount is used in chess engines (counting pieces on a bitboard), network routing (counting subnet mask bits), and error-correction codes.

### Real-World Use Cases

| Pattern | Application |
|---------|-------------|
| Set/clear bits | Hardware register configuration |
| Test bits | Checking file permissions (rwx) |
| Popcount | Hamming distance, error detection |
| Power-of-2 check | Memory allocator alignment |

### Your Task

Write a program that counts the number of set bits (1-bits) in the value 0xB5 (binary: 10110101 = 5 set bits). Print the count followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 2

.text
.global _start
_start:
\tMOV X0, #0xB5
\t// Count the number of 1-bits in X0
\t// Store the count in X1
\t// Convert to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 2

.text
.global _start
_start:
\tMOV X0, #0xB5
\tMOV X1, #0

count_loop:
\tCBZ X0, count_done
\tAND X2, X0, #1
\tADD X1, X1, X2
\tLSR X0, X0, #1
\tB count_loop

count_done:
\tADD X1, X1, #48

\tLDR X3, =buf
\tSTRB W1, [X3]
\tMOV X4, #10
\tSTRB W4, [X3, #1]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #2
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints 5",
      expected: "5\n",
    },
  ],
};
