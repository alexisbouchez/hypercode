import type { Lesson } from "../../types";

export const signedArithmetic: Lesson = {
  id: "signed-arithmetic",
  title: "Signed Arithmetic",
  chapterId: "foundations",
  content: `## Signed Numbers and Negative Values

So far we have only worked with positive (unsigned) numbers. But real programs need to handle negative values too. ARM64 uses **two's complement** representation, the same system used by virtually every modern processor.

### Two's Complement

In two's complement, the most significant bit (bit 63 for 64-bit registers) acts as the **sign bit**:

| Bit 63 | Meaning |
|--------|---------|
| 0 | Positive number (or zero) |
| 1 | Negative number |

To negate a value, you invert all bits and add 1. For example, in 8-bit:
- \`5\` = \`00000101\`
- \`-5\` = \`11111011\` (invert: \`11111010\`, add 1: \`11111011\`)

The beauty of two's complement is that \`ADD\` and \`SUB\` work the same way for signed and unsigned values -- the hardware does not need separate circuits.

> Negative numbers are like the Mirror Universe in Star Trek -- every positive value has an evil twin on the other side of zero, and two's complement is the portal between them.

### NEG -- Negate

\`NEG\` computes the two's complement negation of a register:

\`\`\`asm
MOV X0, #42
NEG X1, X0        // X1 = -42
\`\`\`

This is equivalent to \`SUB X1, XZR, X0\` (subtracting from zero).

### SDIV -- Signed Division

We have used \`UDIV\` for unsigned division. \`SDIV\` performs **signed** division, which handles negative operands correctly:

\`\`\`asm
// Unsigned division:
UDIV X2, X0, X1    // treats all bits as positive magnitude

// Signed division:
SDIV X2, X0, X1    // treats bit 63 as sign bit
\`\`\`

For example, if X0 holds -15 and X1 holds 4:
- \`SDIV\` gives -3 (rounds toward zero)
- \`UDIV\` would interpret -15 as a huge positive number and give a wrong result

### Signed vs Unsigned Comparisons

After \`CMP\`, different branch conditions handle signed vs unsigned:

| Signed | Unsigned | Meaning |
|--------|----------|---------|
| \`B.GT\` | \`B.HI\` | Greater than |
| \`B.GE\` | \`B.HS\` | Greater than or equal |
| \`B.LT\` | \`B.LO\` | Less than |
| \`B.LE\` | \`B.LS\` | Less than or equal |

### Computing Absolute Values

A common pattern with signed numbers is computing absolute values. If a subtraction produces a negative result, you negate it:

\`\`\`asm
SUB X2, X0, X1      // X2 = a - b (might be negative)
CMP X2, #0
B.GE positive        // if X2 >= 0 (signed), skip
NEG X2, X2           // X2 = -X2 (make positive)
positive:
\`\`\`

### Signed Remainder

ARM64 has no modulo instruction. To compute the signed remainder \`a % b\`, use the pattern:

\`\`\`asm
SDIV X2, X0, X1     // X2 = a / b
MUL X3, X2, X1      // X3 = (a / b) * b
SUB X4, X0, X3      // X4 = a - (a / b) * b = a % b
\`\`\`

### Your Task

Given two values \`a = 12\` and \`b = 37\`, compute the absolute difference \`|a - b|\`. Since a < b, subtracting \`a - b\` gives a negative result (-25). Negate it to get 25. Print the result followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #12
\tMOV X1, #37
\t// Compute a - b (will be negative: -25)
\t// Check if result is negative
\t// If so, negate it to get the absolute value
\t// Convert 25 to ASCII digits and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #12
\tMOV X1, #37
\tSUB X2, X0, X1

\tCMP X2, #0
\tB.GE positive
\tNEG X2, X2
positive:

\tMOV X3, #10
\tUDIV X4, X2, X3
\tMUL X5, X4, X3
\tSUB X5, X2, X5

\tADD X4, X4, #48
\tADD X5, X5, #48

\tLDR X6, =buf
\tSTRB W4, [X6]
\tSTRB W5, [X6, #1]
\tMOV X7, #10
\tSTRB W7, [X6, #2]

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
      name: "prints 25",
      expected: "25\n",
    },
  ],
};
