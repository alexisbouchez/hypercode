import type { Lesson } from "../../types";

export const conditionalSelection: Lesson = {
  id: "conditional-selection",
  title: "Conditional Selection",
  chapterId: "control-flow",
  content: `## Branchless Conditional Logic

Branches (B.EQ, B.GT, etc.) work well, but they can be expensive on modern processors because they disrupt the instruction pipeline. ARM64 provides **conditional selection** instructions that choose between two values without branching.

### CSEL -- Conditional Select

\`CSEL\` picks one of two registers based on a condition:

\`\`\`asm
CMP X0, X1
CSEL X2, X3, X4, GT   // if X0 > X1 then X2 = X3 else X2 = X4
\`\`\`

This is like the C ternary operator: \`X2 = (X0 > X1) ? X3 : X4\`.

### CSINC -- Conditional Select and Increment

\`CSINC\` selects the first register if true, or the second register **plus one** if false:

\`\`\`asm
CMP X0, #10
CSINC X1, X2, X3, EQ  // if X0 == 10 then X1 = X2 else X1 = X3 + 1
\`\`\`

A common pattern is using CSINC to create a boolean flag:

\`\`\`asm
CMP X0, X1
CSINC X2, XZR, XZR, NE  // X2 = (X0 == X1) ? 0 : 0 + 1 = 1 if not equal
\`\`\`

Wait -- that sets X2 to 1 when NOT equal, and 0 when equal. To set X2 to 1 when equal, invert the condition:

\`\`\`asm
CMP X0, X1
CSINC X2, XZR, XZR, NE  // X2 = 1 if equal (condition is inverted!)
\`\`\`

This is actually what the \`CSET\` pseudo-instruction does: \`CSET X2, EQ\` is shorthand for \`CSINC X2, XZR, XZR, NE\`.

### CSNEG -- Conditional Select and Negate

\`CSNEG\` selects the first register if true, or the **negation** of the second register if false:

\`\`\`asm
CMP X0, #0
CSNEG X1, X0, X0, GE  // if X0 >= 0 then X1 = X0 else X1 = -X0
\`\`\`

This computes the absolute value of X0 in a single instruction (after CMP)!

### CSINV -- Conditional Select and Invert

\`CSINV\` selects the first register if true, or the **bitwise NOT** of the second register if false:

\`\`\`asm
CMP X0, #0
CSINV X1, X0, X0, GE  // if X0 >= 0 then X1 = X0 else X1 = ~X0
\`\`\`

### Branchless Max and Min

Finding the maximum of two values without any branches:

\`\`\`asm
CMP X0, X1
CSEL X2, X0, X1, GT   // X2 = max(X0, X1)
CSEL X3, X0, X1, LT   // X3 = min(X0, X1)
\`\`\`

### Branchless Clamping

Clamping a value to a range [lo, hi]:

\`\`\`asm
// Clamp X0 to [10, 50]
CMP X0, #10
CSEL X0, X0, X10, GT  // X0 = max(X0, 10) -- where X10 holds 10
CMP X0, #50
CSEL X0, X0, X11, LT  // X0 = min(X0, 50) -- where X11 holds 50
\`\`\`

### Your Task

Compute the absolute value of -17 using \`CSNEG\` (no branches). Load -17 by computing \`0 - 17\`, then use \`CMP\` and \`CSNEG\` to get the absolute value (17). Print it followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #17
\tSUB X0, XZR, X0
\t// X0 now holds -17
\t// Use CMP and CSNEG to compute |X0|
\t// Convert 17 to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #17
\tSUB X0, XZR, X0

\tCMP X0, #0
\tCSNEG X1, X0, X0, GE

\tMOV X3, #10
\tUDIV X4, X1, X3
\tMUL X5, X4, X3
\tSUB X5, X1, X5

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
      name: "prints 17",
      expected: "17\n",
    },
  ],
};
