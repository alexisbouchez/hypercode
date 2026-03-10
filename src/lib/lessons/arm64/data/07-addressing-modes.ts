import type { Lesson } from "../../types";

export const addressingModes: Lesson = {
  id: "addressing-modes",
  title: "Addressing Modes",
  chapterId: "memory",
  content: `## Addressing Modes

ARM64 supports several ways to compute memory addresses. Understanding these modes is key to writing efficient code -- the right addressing mode can save instructions when working with arrays and data structures.

> Like the navigational deflector on the Enterprise, addressing modes give you different ways to plot a course to your data -- each optimized for a different flight plan.

### Summary of Modes

| Mode | Syntax | Effect |
|------|--------|--------|
| Offset | \`[X1, #8]\` | Access X1+8, X1 unchanged |
| Pre-index | \`[X1, #8]!\` | X1 += 8, then access X1 |
| Post-index | \`[X1], #8\` | Access X1, then X1 += 8 |
| Register offset | \`[X1, X2]\` | Access X1+X2, both unchanged |

### Offset Addressing

The base register is not modified. The offset is added to compute the address:

\`\`\`asm
LDR X0, [X1, #16]   // Load from X1+16, X1 unchanged
STR X0, [X1, #8]    // Store to X1+8, X1 unchanged
\`\`\`

This is the most common mode -- ideal for accessing struct fields or array elements at known offsets.

### Pre-Index Addressing

The offset is added to the base register **before** the memory access, and the base register is updated:

\`\`\`asm
LDR X0, [X1, #8]!   // X1 = X1+8, then load from X1
STR X0, [X1, #-16]! // X1 = X1-16, then store to X1
\`\`\`

The \`!\` suffix means "write back" -- the base register is permanently updated. This is commonly used for stack operations (allocating stack space before storing).

### Post-Index Addressing

The memory access uses the base register as-is, **then** the offset is added:

\`\`\`asm
LDR X0, [X1], #8    // Load from X1, then X1 = X1+8
STR X0, [X1], #16   // Store to X1, then X1 = X1+16
\`\`\`

This is perfect for walking through arrays: load the current element and advance the pointer in one instruction.

### Register Offset Addressing

You can use a register as the offset, which is useful for indexing arrays with a variable:

\`\`\`asm
LDRB W0, [X1, X2]   // Load byte from X1 + X2
\`\`\`

### Walking Through an Array with a Loop

Post-index is especially natural for sequential access. Compare these two approaches:

\`\`\`asm
// Without post-index (2 instructions per element):
LDRB W0, [X1]       // Load byte
ADD X1, X1, #1      // Advance pointer

// With post-index (1 instruction per element):
LDRB W0, [X1], #1   // Load byte AND advance pointer
\`\`\`

The real power of post-index shows when combined with a loop. Instead of repeating \`LDRB\`/\`ADD\` for each element, you can process any number of elements with a fixed set of instructions:

\`\`\`asm
MOV X2, #4          // counter = array length
MOV X5, #0          // sum = 0
loop:
    LDRB W1, [X0], #1   // Load byte AND advance pointer
    ADD X5, X5, X1       // Accumulate into sum
    SUBS X2, X2, #1      // counter-- (sets flags)
    B.NE loop            // Repeat if counter != 0
\`\`\`

We are sneaking a peek at two instructions from later lessons:

- **\`SUBS\`** works like \`SUB\` but also sets condition flags (the \`S\` suffix means "set flags"). When the result is zero, it sets the Zero flag.
- **\`B.NE\`** ("Branch if Not Equal") jumps to the label if the Zero flag is **not** set -- i.e., the counter has not reached zero.

Together they form a counted loop: decrement, check for zero, repeat. This pattern scales to arrays of any size -- just change the counter.

> **Tip**: Use post-index for forward iteration and pre-index for stack operations. Offset addressing is best when you know the exact position at compile time.

### Your Task

An array of four bytes is defined in the data section: \`3, 7, 2, 8\`. Using post-index addressing **in a loop**, load each byte, add them all together, convert the sum (20) to ASCII, and print it followed by a newline.`,

  starterCode: `.data
arr:
\t.byte 3, 7, 2, 8
buf:
\t.skip 3

.text
.global _start
_start:
\t// Load address of arr into X0
\t// Set counter X2 = 4 (array length)
\t// Set sum X5 = 0
\t// Loop: LDRB with post-index, ADD to sum, SUBS counter, B.NE loop
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
\tMOV X2, #4
\tMOV X5, #0

loop:
\tLDRB W1, [X0], #1
\tADD X5, X5, X1
\tSUBS X2, X2, #1
\tB.NE loop

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
