import type { Lesson } from "../../types";

export const bitManipulation: Lesson = {
  id: "bit-manipulation",
  title: "Bit Manipulation Patterns",
  chapterId: "bitwise",
  content: `## Common Bit Manipulation Patterns

Bitwise operations enable efficient solutions to many problems. Here are key patterns.

### Set a Bit

Use \`ORR\` with a mask to set (turn on) a specific bit:

\`\`\`asm
MOV X1, #1
LSL X1, X1, #3       // X1 = 1 << 3 = 0b1000
ORR X0, X0, X1       // Set bit 3 of X0
\`\`\`

### Clear a Bit

Use \`AND\` with an inverted mask (\`MVN\`) to clear (turn off) a specific bit:

\`\`\`asm
MOV X1, #1
LSL X1, X1, #3       // X1 = 0b1000
MVN X1, X1           // X1 = ~0b1000 = 0b...0111
AND X0, X0, X1       // Clear bit 3 of X0
\`\`\`

### Toggle a Bit

Use \`EOR\` to flip a specific bit:

\`\`\`asm
MOV X1, #1
LSL X1, X1, #3
EOR X0, X0, X1       // Toggle bit 3
\`\`\`

### Test if a Bit is Set

Use \`AND\` and check if the result is zero:

\`\`\`asm
AND X1, X0, #0x08    // Isolate bit 3
CBZ X1, bit_not_set  // Branch if bit 3 is 0
\`\`\`

### Check if Power of Two

A number is a power of two if it has exactly one bit set: \`n & (n-1) == 0\` and \`n != 0\`:

\`\`\`asm
SUB X1, X0, #1       // X1 = n - 1
AND X1, X0, X1       // X1 = n & (n-1)
CBZ X1, is_power     // If zero, n is a power of 2
\`\`\`

### Counting Set Bits (popcount)

Loop through all bits, testing each one:

\`\`\`asm
MOV X1, #0           // count = 0
count_loop:
    CBZ X0, count_done
    AND X2, X0, #1   // test lowest bit
    ADD X1, X1, X2   // count += bit
    LSR X0, X0, #1   // shift right
    B count_loop
count_done:
\`\`\`

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
