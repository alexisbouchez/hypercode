import type { Lesson } from "../../types";

export const bitwiseOperations: Lesson = {
  id: "bitwise-operations",
  title: "Bitwise Operations",
  chapterId: "bitwise",
  content: `## Bitwise Instructions

Bitwise operations work on individual bits of a value. They are fundamental to low-level programming -- used for hardware registers, network protocols, file permissions, graphics, cryptography, and countless other domains.

> "You will be assimilated. Resistance is futile." -- The Borg see everything as ones and zeros, and so do bitwise operations. Welcome to their world.

### Quick Reference

| Instruction | Operation | C equivalent |
|-------------|-----------|-------------|
| \`AND\` | Bitwise AND | \`a & b\` |
| \`ORR\` | Bitwise OR | \`a \\| b\` |
| \`EOR\` | Bitwise XOR | \`a ^ b\` |
| \`MVN\` | Bitwise NOT | \`~a\` |
| \`LSL\` | Shift left | \`a << n\` |
| \`LSR\` | Logical shift right | \`a >> n\` (unsigned) |
| \`ASR\` | Arithmetic shift right | \`a >> n\` (signed) |

### AND -- Masking Bits

\`AND\` keeps only the bits that are 1 in **both** operands. It is used to extract or isolate specific bits:

\`\`\`asm
AND X0, X1, #0xFF   // Extract the low byte (bits 0-7)
AND X0, X1, #0x0F   // Extract the low nibble (bits 0-3)
\`\`\`

### ORR -- Setting Bits

\`ORR\` turns bits ON. Any bit that is 1 in either operand will be 1 in the result:

\`\`\`asm
ORR X0, X1, #0x80   // Set bit 7 (turn it on)
ORR X0, X1, X2      // Combine flags from both registers
\`\`\`

### EOR -- Toggling Bits

\`EOR\` flips bits. A bit that differs between the two operands becomes 1:

\`\`\`asm
EOR X0, X1, #0xFF   // Toggle all bits in the low byte
EOR X0, X0, X0      // Clear a register (x XOR x = 0)
\`\`\`

### Shift Instructions

Shifts move bits left or right, filling the vacated positions with zeros (or sign bits for ASR):

\`\`\`asm
LSL X0, X1, #4      // Shift left by 4: multiply by 16
LSR X0, X1, #4      // Shift right by 4: unsigned divide by 16
ASR X0, X1, #4      // Arithmetic shift right: signed divide by 16
\`\`\`

\`LSL\` by N multiplies by 2^N. \`LSR\` by N divides by 2^N. These are much faster than MUL/UDIV for powers of 2.

### Practical Example: Extracting Nibbles

A **nibble** is 4 bits (half a byte). Each nibble maps to one hex digit. To split a byte into its two hex digits:

\`\`\`asm
LSR X2, X0, #4      // High nibble: shift right by 4
AND X3, X0, #0x0F   // Low nibble: mask bottom 4 bits
\`\`\`

### Your Task

Write a program that uses bitwise operations to:
1. Start with the value 0xCA (202)
2. Shift right by 4 to get the high nibble (0x0C = 12 = 'C')
3. AND with 0x0F to extract the low nibble (0x0A = 10 = 'A')
4. Print both as hex digits: \`CA\\n\`

Hint: To convert 0-15 to a hex digit character, add 48 for 0-9 and add 55 for A-F (10 + 55 = 65 = 'A').`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start

_start:
\tMOV X0, #0xCA
\t// Extract high nibble (C = 12)
\t// Extract low nibble (A = 10)
\t// Convert each to hex ASCII and store in buf
\t// Print and exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start

to_hex:
\tCMP X0, #10
\tB.GE hex_letter
\tADD X0, X0, #48
\tRET
hex_letter:
\tADD X0, X0, #55
\tRET

_start:
\tMOV X19, #0xCA

\tLSR X0, X19, #4
\tBL to_hex
\tLDR X4, =buf
\tSTRB W0, [X4]

\tAND X0, X19, #0x0F
\tBL to_hex
\tLDR X4, =buf
\tSTRB W0, [X4, #1]

\tMOV X5, #10
\tSTRB W5, [X4, #2]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #3
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints CA",
      expected: "CA\n",
    },
  ],
};
