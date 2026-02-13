import type { Lesson } from "../../types";

export const conditionalBranches: Lesson = {
  id: "conditional-branches",
  title: "Conditional Branches",
  chapterId: "control-flow",
  content: `## Conditional Branches

After setting the flags with \`CMP\`, you use conditional branch instructions to control program flow. This is how assembly implements \`if\`, \`else\`, \`switch\`, and every other decision-making construct.

### B.cond -- Branch if Condition

The syntax is \`B.\` followed by a condition code:

| Instruction | Meaning (after CMP a, b) | Use for |
|-------------|--------------------------|---------|
| \`B.EQ\` | a == b | Equality check |
| \`B.NE\` | a != b | Inequality check |
| \`B.GT\` | a > b (signed) | Signed greater than |
| \`B.GE\` | a >= b (signed) | Signed greater or equal |
| \`B.LT\` | a < b (signed) | Signed less than |
| \`B.LE\` | a <= b (signed) | Signed less or equal |
| \`B.HI\` | a > b (unsigned) | Unsigned greater than |
| \`B.LO\` | a < b (unsigned) | Unsigned less than |

> **Tip**: Use the signed variants (\`GT\`, \`GE\`, \`LT\`, \`LE\`) for most comparisons. Use unsigned variants (\`HI\`, \`LO\`) when comparing addresses or values that cannot be negative.

### Implementing if/else

The pattern is: compare, branch over the "else" code, then skip over the "then" code:

\`\`\`asm
CMP X0, #10
B.GT greater          // if X0 > 10, jump to "greater"
// else branch (X0 <= 10):
// ... code ...
B done                // skip over the "greater" branch
greater:
// then branch (X0 > 10):
// ... code ...
done:
\`\`\`

> **Key insight**: Assembly \`if/else\` is "inside out" compared to high-level code. You branch to skip code, not to run it. The condition in \`B.GT\` is the *opposite* of what you would write as the \`else\` condition.

### Unconditional Branch

\`B label\` always jumps to the label:

\`\`\`asm
B skip_this           // always jump
// this code is skipped
skip_this:
\`\`\`

### CBZ and CBNZ

These combine a compare-to-zero and branch in one instruction. They are very common in loops:

\`\`\`asm
CBZ X0, is_zero       // Branch if X0 == 0
CBNZ X0, not_zero     // Branch if X0 != 0
\`\`\`

These do **not** set flags and do not require a prior \`CMP\` instruction -- they test the register directly.

### Finding Maximum -- The Pattern

A common pattern for finding the maximum of N values:

\`\`\`asm
MOV X3, X0           // max = first value
CMP X1, X3           // compare second with max
B.LE skip1           // if second <= max, skip
MOV X3, X1           // max = second
skip1:
// repeat for each additional value...
\`\`\`

### Your Task

Write a program that finds the maximum of three numbers: 17, 42, and 29. Store each in a register, compare them using \`CMP\` and conditional branches, and print the maximum (42) followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #17
\tMOV X1, #42
\tMOV X2, #29

\t// Find the max of X0, X1, X2
\t// Store the result in X3
\t// Convert to ASCII and print

\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #17
\tMOV X1, #42
\tMOV X2, #29

\tMOV X3, X0
\tCMP X1, X3
\tB.LE skip1
\tMOV X3, X1
skip1:
\tCMP X2, X3
\tB.LE skip2
\tMOV X3, X2
skip2:

\tMOV X1, #10
\tUDIV X4, X3, X1
\tMUL X5, X4, X1
\tSUB X5, X3, X5

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
      name: "prints 42",
      expected: "42\n",
    },
  ],
};
