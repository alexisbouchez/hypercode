import type { Lesson } from "../../types";

export const flagsAndCmp: Lesson = {
  id: "flags-and-cmp",
  title: "Flags and CMP",
  chapterId: "control-flow",
  content: `## Condition Flags

ARM64 has four condition flags in the NZCV register:

| Flag | Name | Set when... |
|------|------|-------------|
| **N** | Negative | Result is negative (bit 63 = 1) |
| **Z** | Zero | Result is zero |
| **C** | Carry | Unsigned overflow occurred |
| **V** | oVerflow | Signed overflow occurred |

These flags are **not** set by normal instructions like \`ADD\` and \`SUB\`. To set flags, you need to use the flag-setting variants: \`ADDS\`, \`SUBS\`, or the \`CMP\` instruction.

### CMP -- Compare

\`CMP\` subtracts its second operand from its first and sets the flags, but discards the result:

\`\`\`asm
CMP X0, #10      // Computes X0 - 10, sets flags
CMP X0, X1       // Computes X0 - X1, sets flags
\`\`\`

After \`CMP\`:
- **Z=1** if the values are equal
- **N=1** if the first value is less (signed)
- **C=1** if the first value is greater or equal (unsigned)

### CMN -- Compare Negative

\`CMN\` adds its operands and sets flags (useful for comparing against negative values):

\`\`\`asm
CMN X0, #5       // Computes X0 + 5, sets flags
\`\`\`

### ADDS and SUBS

These are flag-setting versions of ADD and SUB:

\`\`\`asm
ADDS X0, X1, X2   // X0 = X1 + X2, sets flags
SUBS X0, X1, X2   // X0 = X1 - X2, sets flags
\`\`\`

### Your Task

Write a program that compares two values (15 and 15). If they are equal, print \`EQ\\n\`. If not, print \`NE\\n\`. Use \`CMP\` and a conditional branch (\`B.NE\`, which we'll cover next lesson -- but use it here as a preview).

Hint: After \`CMP X0, X1\`, use \`B.NE not_equal\` to skip to a different label if they are not equal.`,

  starterCode: `.data
eq_msg:
\t.ascii "EQ\\n"
ne_msg:
\t.ascii "NE\\n"

.text
.global _start
_start:
\tMOV X0, #15
\tMOV X1, #15
\t// Compare X0 and X1
\t// Branch to not_equal if they differ
\t// Otherwise fall through to print "EQ"

\t// Print "EQ\\n" and exit

not_equal:
\t// Print "NE\\n" and exit
`,

  solution: `.data
eq_msg:
\t.ascii "EQ\\n"
ne_msg:
\t.ascii "NE\\n"

.text
.global _start
_start:
\tMOV X0, #15
\tMOV X1, #15
\tCMP X0, X1
\tB.NE not_equal

\tMOV X0, #1
\tLDR X1, =eq_msg
\tMOV X2, #3
\tMOV X8, #64
\tSVC #0
\tB done

not_equal:
\tMOV X0, #1
\tLDR X1, =ne_msg
\tMOV X2, #3
\tMOV X8, #64
\tSVC #0

done:
\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints EQ for equal values",
      expected: "EQ\n",
    },
  ],
};
