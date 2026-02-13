import type { Lesson } from "../../types";

export const addressingModes: Lesson = {
  id: "addressing-modes",
  title: "Addressing Modes",
  chapterId: "memory",
  content: `## Addressing Modes

ARM64 supports several ways to compute memory addresses. The three main addressing modes are offset, pre-index, and post-index.

### Offset Addressing

The base register is not modified. The offset is added to compute the address:

\`\`\`asm
LDR X0, [X1, #16]   // Load from X1+16, X1 unchanged
STR X0, [X1, #8]    // Store to X1+8, X1 unchanged
\`\`\`

### Pre-Index Addressing

The offset is added to the base register **before** the memory access, and the base register is updated:

\`\`\`asm
LDR X0, [X1, #8]!   // X1 = X1+8, then load from X1
STR X0, [X1, #-16]! // X1 = X1-16, then store to X1
\`\`\`

The \`!\` suffix means "write back" -- the base register is updated with the computed address.

### Post-Index Addressing

The memory access uses the base register as-is, then the offset is added to the base register:

\`\`\`asm
LDR X0, [X1], #8    // Load from X1, then X1 = X1+8
STR X0, [X1], #16   // Store to X1, then X1 = X1+16
\`\`\`

### LDRB and STRB

\`LDRB\` and \`STRB\` load and store single bytes:

\`\`\`asm
LDRB W0, [X1]       // Load 1 byte into W0 (zero-extended)
STRB W0, [X1]       // Store low byte of W0
\`\`\`

### Walking Through an Array

Pre-index and post-index modes are useful for iterating through arrays. Post-index is especially natural for sequential access:

\`\`\`asm
LDR X0, [X1], #8    // Load value, advance pointer by 8
\`\`\`

### Your Task

An array of four bytes is defined in the data section: \`3, 7, 2, 8\`. Using post-index addressing, load each byte, add them all together, convert the sum (20) to ASCII, and print it followed by a newline.`,

  starterCode: `.data
arr:
\t.byte 3, 7, 2, 8
buf:
\t.skip 3

.text
.global _start
_start:
\t// Load address of arr into X0
\t// Use post-index LDRB to load each byte
\t// Sum them into X5
\t// Convert sum to ASCII and print
\t// Exit
`,

  solution: `.data
arr:
\t.byte 3, 7, 2, 8
buf:
\t.skip 3

.text
.global _start
_start:
\tLDR X0, =arr
\tMOV X5, #0

\tLDRB W1, [X0], #1
\tADD X5, X5, X1
\tLDRB W1, [X0], #1
\tADD X5, X5, X1
\tLDRB W1, [X0], #1
\tADD X5, X5, X1
\tLDRB W1, [X0], #1
\tADD X5, X5, X1

\tMOV X1, #10
\tUDIV X2, X5, X1
\tMUL X3, X2, X1
\tSUB X3, X5, X3

\tADD X2, X2, #48
\tADD X3, X3, #48

\tLDR X4, =buf
\tSTRB W2, [X4]
\tSTRB W3, [X4, #1]
\tMOV X6, #10
\tSTRB W6, [X4, #2]

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
      name: "prints 20",
      expected: "20\n",
    },
  ],
};
