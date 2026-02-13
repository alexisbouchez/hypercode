import type { Lesson } from "../../types";

export const registersAndMov: Lesson = {
  id: "registers-and-mov",
  title: "Registers and MOV",
  chapterId: "foundations",
  content: `## Moving Data Around

The \`MOV\` instruction copies a value into a register. It is the most fundamental ARM64 instruction -- almost every program starts by moving values into registers.

### MOV with Immediates

You can move a constant value (an "immediate") directly into a register:

\`\`\`asm
MOV X0, #42        // X0 = 42
MOV X1, #0xFF      // X1 = 255
MOV X2, #0b1010    // X2 = 10 (binary)
\`\`\`

The \`#\` prefix indicates an immediate value. You can use decimal, hex (\`0x\`), or binary (\`0b\`) notation.

### MOV Between Registers

You can copy a value from one register to another:

\`\`\`asm
MOV X0, #10        // X0 = 10
MOV X1, X0         // X1 = 10 (copied from X0)
\`\`\`

> **Important**: \`MOV\` copies the value -- it does not move it. After \`MOV X1, X0\`, both X0 and X1 contain the same value.

### X Registers vs W Registers

Each 64-bit \`X\` register has a 32-bit alias called \`W\`:

| 64-bit | 32-bit | Bits used |
|--------|--------|-----------|
| \`X0\` | \`W0\` | Full 64 bits vs lower 32 bits |
| \`X1\` | \`W1\` | Full 64 bits vs lower 32 bits |

Writing to a \`W\` register **zero-extends** the result to 64 bits -- the upper 32 bits are automatically cleared:

\`\`\`asm
MOV X0, #0xFFFFFFFF00000000  // X0 has upper bits set
MOV W0, #5                    // X0 = 5 (upper 32 bits cleared!)
\`\`\`

You will use \`W\` registers when working with bytes (\`STRB W0\`) and 32-bit values.

### MOVZ and MOVK

ARM64 instructions are 32 bits wide, so you cannot always encode a large immediate in a single \`MOV\`. For larger values, ARM64 provides:

- \`MOVZ\` -- Move with Zero: loads a 16-bit immediate into a register, clearing all other bits
- \`MOVK\` -- Move with Keep: loads a 16-bit immediate into a specific position, keeping other bits unchanged

You specify the position with \`LSL #n\` where n is 0, 16, 32, or 48:

\`\`\`asm
MOVZ X0, #0x1234              // X0 = 0x0000000000001234
MOVK X0, #0x5678, LSL #16    // X0 = 0x0000000056781234
\`\`\`

This is how the assembler handles large constants -- \`MOV X0, #0x56781234\` is actually assembled into a MOVZ + MOVK pair behind the scenes.

### The Zero Register

\`XZR\` always reads as zero and discards writes. It is useful as a source of zero:

\`\`\`asm
MOV X0, XZR        // X0 = 0 (same as MOV X0, #0)
\`\`\`

Writing to \`XZR\` is a no-op -- the value is thrown away. This is useful for instructions that set flags as a side effect (like \`CMP\`, which is really \`SUBS XZR, Xn, Xm\`).

### Your Task

Write a program that:
1. Loads the value \`72\` into X0 (ASCII 'H')
2. Loads the value \`105\` into X1 (ASCII 'i')
3. Stores them into memory and prints \`Hi\\n\`

Use the data section for the newline, and store the character bytes into a buffer before printing.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\t// Load ASCII 'H' (72) into X0
\t// Load ASCII 'i' (105) into X1
\t// Load address of buf into X2
\t// Store H and i into buf using STRB
\t// Store newline (10) into buf+2
\t// Print buf (3 bytes) using write syscall
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #72
\tMOV X1, #105
\tMOV X3, #10
\tLDR X2, =buf
\tSTRB W0, [X2]
\tSTRB W1, [X2, #1]
\tSTRB W3, [X2, #2]

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
      name: "prints Hi",
      expected: "Hi\n",
    },
  ],
};
