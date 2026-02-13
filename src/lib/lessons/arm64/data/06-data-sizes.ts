import type { Lesson } from "../../types";

export const dataSizes: Lesson = {
  id: "data-sizes",
  title: "Data Sizes",
  chapterId: "memory",
  content: `## Working with Different Data Sizes

Real-world data comes in different sizes. ARM64 provides load and store instructions for each size:

| Size | Bits | Load | Store | Directive |
|------|------|------|-------|-----------|
| Byte | 8 | \`LDRB\` | \`STRB\` | \`.byte\` |
| Halfword | 16 | \`LDRH\` | \`STRH\` | \`.hword\` |
| Word | 32 | \`LDR W\` | \`STR W\` | \`.word\` |
| Doubleword | 64 | \`LDR X\` | \`STR X\` | \`.quad\` |

### LDRH and STRH -- Halfword (16-bit)

\`LDRH\` loads a 16-bit value from memory, zero-extending it to fill the register. \`STRH\` stores the lower 16 bits of a register:

\`\`\`asm
LDRH W0, [X1]         // Load 16-bit value from [X1] into W0
STRH W0, [X1, #2]     // Store low 16 bits of W0 to [X1+2]
\`\`\`

Halfwords are 2 bytes, so consecutive halfwords are spaced 2 bytes apart.

### The .hword Directive

The \`.hword\` directive stores 16-bit values (2 bytes each) in the data section:

\`\`\`asm
.data
values:
    .hword 100, 200, 300    // Three 16-bit values
\`\`\`

Each \`.hword\` value occupies 2 bytes. You access them using \`LDRH\`:

\`\`\`asm
LDR X0, =values
LDRH W1, [X0]          // W1 = 100
LDRH W2, [X0, #2]      // W2 = 200 (2 bytes later)
LDRH W3, [X0, #4]      // W3 = 300 (4 bytes later)
\`\`\`

### The .word Directive

The \`.word\` directive stores 32-bit values (4 bytes each):

\`\`\`asm
.data
counts:
    .word 100000, 200000    // Two 32-bit values
\`\`\`

Each \`.word\` value occupies 4 bytes. You access them using W registers:

\`\`\`asm
LDR X0, =counts
LDR W1, [X0]          // W1 = 100000
LDR W2, [X0, #4]      // W2 = 200000 (4 bytes later)
\`\`\`

### Zero Extension vs Sign Extension

When loading a value smaller than 64 bits, the upper bits must be filled somehow:

- **Zero extension** (LDRB, LDRH, LDR W): Upper bits are set to 0. Used for unsigned values.
- **Sign extension** (LDRSW): The sign bit is copied into the upper bits. Used for signed values.

\`\`\`asm
// If memory at [X1] contains 0xFF80 (65408 unsigned, -128 signed):
LDRH W0, [X1]         // W0 = 0x0000FF80 (zero-extended, positive)
// LDRSW sign-extends 32-bit values to 64 bits.
\`\`\`

### Practical Use Case: Summing an Array

Arrays of 16-bit values are common in audio processing, sensor data, and network protocols. To sum 16-bit values:

\`\`\`asm
LDR X0, =halfwords    // pointer to array
MOV X1, #0            // sum = 0
MOV X2, #4            // count = 4
loop:
    CBZ X2, done
    LDRH W3, [X0], #2 // load halfword, advance pointer by 2
    ADD X1, X1, X3    // sum += value
    SUB X2, X2, #1
    B loop
done:
\`\`\`

### Your Task

An array of four 16-bit halfword values is defined: \`100, 200, 300, 400\`. Load each value using \`LDRH\`, sum them (the total is 1000), and print the result followed by a newline.

Hint: You will need to convert a 4-digit number (1000) to ASCII. Extract each digit by dividing by 1000, 100, 10, and using the remainder.`,

  starterCode: `.data
values:
\t.hword 100, 200, 300, 400
buf:
\t.skip 5

.text
.global _start
_start:
\t// Load address of values
\t// Loop: LDRH each halfword, sum them
\t// Convert sum (1000) to ASCII digits
\t// Print and exit
`,

  solution: `.data
values:
\t.hword 100, 200, 300, 400
buf:
\t.skip 5

.text
.global _start
_start:
\tLDR X0, =values
\tMOV X1, #0
\tMOV X2, #4

sum_loop:
\tCBZ X2, sum_done
\tLDRH W3, [X0], #2
\tADD X1, X1, X3
\tSUB X2, X2, #1
\tB sum_loop

sum_done:
\tMOV X10, X1

\tMOV X3, #1000
\tUDIV X4, X10, X3
\tMUL X5, X4, X3
\tSUB X10, X10, X5

\tMOV X3, #100
\tUDIV X5, X10, X3
\tMUL X6, X5, X3
\tSUB X10, X10, X6

\tMOV X3, #10
\tUDIV X6, X10, X3
\tMUL X7, X6, X3
\tSUB X7, X10, X7

\tADD X4, X4, #48
\tADD X5, X5, #48
\tADD X6, X6, #48
\tADD X7, X7, #48

\tLDR X9, =buf
\tSTRB W4, [X9]
\tSTRB W5, [X9, #1]
\tSTRB W6, [X9, #2]
\tSTRB W7, [X9, #3]
\tMOV W11, #10
\tSTRB W11, [X9, #4]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #5
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints 1000",
      expected: "1000\n",
    },
  ],
};
