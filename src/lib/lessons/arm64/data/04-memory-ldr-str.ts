import type { Lesson } from "../../types";

export const memoryLdrStr: Lesson = {
  id: "memory-ldr-str",
  title: "Memory: LDR and STR",
  chapterId: "memory",
  content: `## Loading and Storing Data

ARM64 is a load/store architecture: you cannot operate directly on memory. All computation happens in registers, and you use \`LDR\` (load) and \`STR\` (store) to move data between registers and memory.

### LDR -- Load Register

\`LDR\` reads a value from memory into a register:

\`\`\`asm
LDR X0, [X1]        // Load 8 bytes from address in X1 into X0
LDR X0, [X1, #8]    // Load from address X1+8
LDR W0, [X1]        // Load 4 bytes (W registers = 32-bit)
\`\`\`

### STR -- Store Register

\`STR\` writes a register value to memory:

\`\`\`asm
STR X0, [X1]        // Store X0 at address in X1
STR X0, [X1, #16]   // Store at address X1+16
\`\`\`

### The .data Section

The \`.data\` section holds initialized data. You can define various types:

\`\`\`asm
.data
myByte:   .byte 42         // 1 byte
myWord:   .word 100000     // 4 bytes
myQuad:   .quad 123456789  // 8 bytes
myStr:    .ascii "hello"   // string (no null terminator)
\`\`\`

### Loading Addresses with LDR =

To get the address of a data label, use the \`LDR Xn, =label\` pseudo-instruction:

\`\`\`asm
LDR X0, =myQuad     // X0 = address of myQuad
LDR X1, [X0]        // X1 = value at myQuad (123456789)
\`\`\`

### Your Task

Define two 8-byte values (\`.quad\`) in the data section: \`val_a\` with value 25 and \`val_b\` with value 17. Load both values, add them, convert the result (42) to ASCII, and print it followed by a newline.`,

  starterCode: `.data
val_a:
\t.quad 25
val_b:
\t.quad 17
buf:
\t.skip 3

.text
.global _start
_start:
\t// Load val_a into X0
\t// Load val_b into X1
\t// Add them
\t// Convert result to two ASCII digits
\t// Store in buf and print with newline
\t// Exit
`,

  solution: `.data
val_a:
\t.quad 25
val_b:
\t.quad 17
buf:
\t.skip 3

.text
.global _start
_start:
\tLDR X0, =val_a
\tLDR X0, [X0]
\tLDR X1, =val_b
\tLDR X1, [X1]
\tADD X0, X0, X1

\tMOV X1, #10
\tUDIV X2, X0, X1
\tMUL X3, X2, X1
\tSUB X3, X0, X3

\tADD X2, X2, #48
\tADD X3, X3, #48

\tLDR X4, =buf
\tSTRB W2, [X4]
\tSTRB W3, [X4, #1]
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
      name: "prints 42",
      expected: "42\n",
    },
  ],
};
