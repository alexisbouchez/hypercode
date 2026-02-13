import type { Lesson } from "../../types";

export const bitwiseOperations: Lesson = {
  id: "bitwise-operations",
  title: "Bitwise Operations",
  chapterId: "bitwise",
  content: `## Bitwise Instructions

Bitwise operations work on individual bits of a value. They are fundamental to low-level programming, hardware interaction, and efficient algorithms.

### AND -- Bitwise AND

Each bit of the result is 1 only if **both** corresponding input bits are 1:

\`\`\`asm
AND X0, X1, X2      // X0 = X1 & X2
AND X0, X1, #0xFF   // X0 = X1 & 0xFF (mask low byte)
\`\`\`

Common use: masking bits to extract specific fields.

### ORR -- Bitwise OR

Each bit of the result is 1 if **either** input bit is 1:

\`\`\`asm
ORR X0, X1, X2      // X0 = X1 | X2
ORR X0, X1, #0x80   // Set bit 7
\`\`\`

Common use: setting specific bits.

### EOR -- Bitwise Exclusive OR

Each bit of the result is 1 if the input bits are **different**:

\`\`\`asm
EOR X0, X1, X2      // X0 = X1 ^ X2
\`\`\`

Common use: toggling bits, simple encryption, swapping values.

### MVN -- Bitwise NOT

Inverts every bit:

\`\`\`asm
MVN X0, X1           // X0 = ~X1
\`\`\`

### Shift Instructions

\`\`\`asm
LSL X0, X1, #4      // Logical shift left: X0 = X1 << 4
LSR X0, X1, #4      // Logical shift right: X0 = X1 >> 4 (unsigned)
ASR X0, X1, #4      // Arithmetic shift right: X0 = X1 >> 4 (preserves sign)
\`\`\`

- \`LSL\` by N is equivalent to multiplying by 2^N
- \`LSR\` by N is equivalent to unsigned division by 2^N
- \`ASR\` by N is equivalent to signed division by 2^N

### Your Task

Write a program that uses bitwise operations to:
1. Start with the value 0xCA (202)
2. AND it with 0x0F to extract the low nibble (should be 0x0A = 10)
3. Shift the original value right by 4 to get the high nibble (should be 0x0C = 12)
4. Print both nibbles as hex digits: \`CA\\n\`

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
