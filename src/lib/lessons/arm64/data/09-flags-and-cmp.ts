import type { Lesson } from "../../types";

export const flagsAndCmp: Lesson = {
  id: "flags-and-cmp",
  title: "Flags and CMP",
  chapterId: "control-flow",
  content: `## Condition Flags

ARM64 uses condition flags to make decisions. These four flags, stored in the NZCV register, are the foundation of all conditional logic -- from simple if/else to complex loops.

### The NZCV Flags

| Flag | Name | Set when... | Example |
|------|------|-------------|---------|
| **N** | Negative | Result bit 63 = 1 | \`5 - 10\` sets N=1 |
| **Z** | Zero | Result is zero | \`5 - 5\` sets Z=1 |
| **C** | Carry | Unsigned overflow | \`0xFFFFFFFF + 1\` sets C=1 |
| **V** | oVerflow | Signed overflow | \`0x7FFFFFFF + 1\` sets V=1 |

> **Important**: Regular \`ADD\` and \`SUB\` do **not** set flags. Only flag-setting instructions (\`CMP\`, \`ADDS\`, \`SUBS\`) modify NZCV. This means you can safely do arithmetic without accidentally changing flags set by a previous comparison.

> Condition flags are the sensor readings of the CPU -- after every comparison, they report the status of your last operation, just like the Enterprise's sensors report conditions ahead.

### CMP -- Compare

\`CMP\` is the primary comparison instruction. It subtracts its second operand from its first, sets the flags, and **discards** the subtraction result:

\`\`\`asm
CMP X0, #10      // Computes X0 - 10, sets flags, result discarded
CMP X0, X1       // Computes X0 - X1, sets flags, result discarded
\`\`\`

After \`CMP X0, X1\`:
- **Z=1** if X0 == X1 (the subtraction is zero)
- **N=1** if X0 < X1 (signed -- the result is negative)
- **C=1** if X0 >= X1 (unsigned -- no borrow occurred)

Internally, \`CMP X0, X1\` is encoded as \`SUBS XZR, X0, X1\` -- a subtraction into the zero register with flag-setting.

### CMN -- Compare Negative

\`CMN\` adds its operands and sets flags. It is useful when comparing against a negative value:

\`\`\`asm
CMN X0, #5       // Computes X0 + 5, sets flags
\`\`\`

### ADDS and SUBS

These are flag-setting versions of ADD and SUB that keep the result:

\`\`\`asm
ADDS X0, X1, X2   // X0 = X1 + X2, AND sets flags
SUBS X0, X1, X2   // X0 = X1 - X2, AND sets flags
\`\`\`

Use these when you need both the result and the flags.

### Your Task

Write a program that compares two values (15 and 15). If they are equal, print \`EQ\\n\`. If not, print \`NE\\n\`. Use \`CMP\` and a conditional branch (\`B.NE\`, which we will cover in the next lesson -- but use it here as a preview).

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
