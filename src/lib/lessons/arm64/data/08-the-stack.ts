import type { Lesson } from "../../types";

export const theStack: Lesson = {
  id: "the-stack",
  title: "The Stack",
  chapterId: "memory",
  content: `## The Stack

The stack is a region of memory used for temporary storage. It grows **downward** -- pushing data decreases the stack pointer, and popping increases it. The stack is essential for function calls, local variables, and saving registers.

### The SP Register

\`SP\` (Stack Pointer) holds the address of the top of the stack.

> **Important**: On AArch64, SP must always be **16-byte aligned**. This means you must always adjust SP in multiples of 16. The hardware will fault if you violate this rule.

### Stack Layout

\`\`\`
High address   ┌──────────────────┐
               │  Previous data   │
               ├──────────────────┤ ← SP (before push)
               │  Saved X29 (FP)  │
               ├──────────────────┤
               │  Saved X30 (LR)  │
               ├──────────────────┤ ← SP (after push)
               │                  │
Low address    └──────────────────┘
\`\`\`

### STP -- Store Pair

\`STP\` stores two registers to memory in a single instruction. It is the standard way to push values onto the stack:

\`\`\`asm
STP X29, X30, [SP, #-16]!  // Push FP and LR onto stack
\`\`\`

This subtracts 16 from SP (pre-index), then stores X29 at [SP] and X30 at [SP+8]. The pre-index \`!\` is critical -- it allocates space before writing.

### LDP -- Load Pair

\`LDP\` loads two registers from memory. It is the standard way to pop values:

\`\`\`asm
LDP X29, X30, [SP], #16    // Pop FP and LR from stack
\`\`\`

This loads X29 from [SP] and X30 from [SP+8], then adds 16 to SP (post-index), freeing the space.

### Stack Frame Pattern

Every function that calls other functions follows this pattern:

\`\`\`asm
my_function:
    // Prologue: save registers
    STP X29, X30, [SP, #-16]!

    // ... function body ...
    // Can safely call other functions here

    // Epilogue: restore registers
    LDP X29, X30, [SP], #16
    RET
\`\`\`

### Saving Multiple Register Pairs

If you need to save more than two registers, adjust SP once and use offset addressing:

\`\`\`asm
STP X19, X20, [SP, #-32]!  // Allocate 32 bytes, save first pair
STP X21, X22, [SP, #16]    // Save second pair at SP+16
// ... use X19-X22 freely ...
LDP X21, X22, [SP, #16]    // Restore second pair
LDP X19, X20, [SP], #32    // Restore first pair, free 32 bytes
\`\`\`

> **Common mistake**: Forgetting to restore registers in the reverse order, or mismatching the STP/LDP offsets. Always pop in the opposite order of pushing.

### Your Task

Write a program that:
1. Pushes the values 10 and 20 onto the stack using STP
2. Pops them back into different registers using LDP
3. Adds the two values
4. Prints the result (30) followed by a newline`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\t// Store 10 and 20 into X0 and X1
\t// Push X0 and X1 onto the stack using STP
\t// Pop them into X2 and X3 using LDP
\t// Add X2 and X3
\t// Convert result to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #10
\tMOV X1, #20
\tSTP X0, X1, [SP, #-16]!
\tLDP X2, X3, [SP], #16
\tADD X4, X2, X3

\tMOV X1, #10
\tUDIV X2, X4, X1
\tMUL X3, X2, X1
\tSUB X3, X4, X3

\tADD X2, X2, #48
\tADD X3, X3, #48

\tLDR X5, =buf
\tSTRB W2, [X5]
\tSTRB W3, [X5, #1]
\tMOV X6, #10
\tSTRB W6, [X5, #2]

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
      name: "prints 30",
      expected: "30\n",
    },
  ],
};
