import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "foundations",
  content: `## Arithmetic Instructions

ARM64 provides instructions for basic arithmetic operations. These are the workhorses of any program -- almost every computation boils down to adds, subtracts, and multiplies.

### ADD and SUB

\`ADD\` adds two values, \`SUB\` subtracts:

\`\`\`asm
ADD X0, X1, X2      // X0 = X1 + X2
ADD X0, X1, #10     // X0 = X1 + 10
SUB X0, X1, X2      // X0 = X1 - X2
SUB X0, X1, #5      // X0 = X1 - 5
\`\`\`

The first operand is the destination, the second is the first source, and the third is either a register or an immediate. The destination can be the same as a source:

\`\`\`asm
ADD X0, X0, #1      // X0 = X0 + 1 (increment)
\`\`\`

### MUL

\`MUL\` multiplies two registers:

\`\`\`asm
MUL X0, X1, X2      // X0 = X1 * X2
\`\`\`

Note: \`MUL\` only takes register operands, not immediates. To multiply by a constant, load it into a register first.

### UDIV -- Unsigned Division

\`UDIV\` performs unsigned integer division (truncates toward zero):

\`\`\`asm
UDIV X0, X1, X2     // X0 = X1 / X2 (integer division)
\`\`\`

ARM64 has no modulo (remainder) instruction. To compute the remainder, use the pattern:

\`\`\`asm
UDIV X2, X0, X1     // quotient = X0 / X1
MUL  X3, X2, X1     // temp = quotient * divisor
SUB  X4, X0, X3     // remainder = X0 - temp
\`\`\`

For example, to get 58 % 10 = 8: divide 58/10 = 5, then 58 - 5*10 = 8.

### Converting Numbers to ASCII Digits

To print a number, you need to convert it to ASCII characters. The ASCII code for the digit '0' is 48. So to convert a single digit (0-9) to its ASCII character, add 48:

\`\`\`asm
MOV X0, #7          // the number 7
ADD X0, X0, #48     // now 55, which is ASCII '7'
\`\`\`

For a two-digit number like 58, extract each digit with division and remainder:

\`\`\`asm
MOV X1, #10
UDIV X2, X0, X1     // X2 = 58 / 10 = 5 (tens digit)
MUL X3, X2, X1      // X3 = 5 * 10 = 50
SUB X3, X0, X3      // X3 = 58 - 50 = 8 (ones digit)
ADD X2, X2, #48     // '5'
ADD X3, X3, #48     // '8'
\`\`\`

> **Tip**: This divide-multiply-subtract pattern for computing remainders is one of the most common patterns in assembly. You will use it in nearly every lesson that prints numbers.

### Your Task

Compute \`7 * 8 + 2\`, convert the result (58) to a two-digit ASCII string, and print it followed by a newline.

The result is 58, so you need to print the characters '5', '8', and '\\n'.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\t// Compute 7 * 8 + 2
\tMOV X0, #7
\tMOV X1, #8
\t// Multiply X0 and X1, store result
\t// Add 2 to the result
\t// Convert to ASCII digits and store in buf
\t// Print buf (3 bytes: two digits + newline)
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #7
\tMOV X1, #8
\tMUL X0, X0, X1
\tADD X0, X0, #2

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
      name: "prints 58",
      expected: "58\n",
    },
  ],
};
