import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## Loops in Assembly

ARM64 has no dedicated loop instruction. Loops are built from the same branches and comparisons you already know -- but combined in specific patterns.

### While Loop Pattern

The standard while loop tests the condition first, then executes the body:

\`\`\`asm
// while (i < N):
loop:
    CMP X0, X1
    B.GE done        // if i >= N, exit
    // ... loop body ...
    ADD X0, X0, #1   // i++
    B loop           // repeat
done:
\`\`\`

This is the most common loop structure. The condition check at the top ensures the body is skipped entirely if the condition is false initially.

### Do-While Loop Pattern

A do-while loop executes the body first, then tests. This is useful when you know the loop runs at least once:

\`\`\`asm
// do { ... } while (counter > 0):
loop:
    // ... loop body (runs at least once) ...
    SUB X0, X0, #1   // counter--
    CBNZ X0, loop    // repeat if counter != 0
\`\`\`

\`CBNZ\` (Compare and Branch if Not Zero) is perfect here -- it combines the test and branch into one instruction.

### For Loop Pattern

\`for (i = 1; i <= N; i++)\`:

\`\`\`asm
MOV X0, #1           // i = 1
MOV X1, #0           // sum = 0
MOV X2, #10          // N = 10
loop:
    CMP X0, X2
    B.GT done        // if i > N, exit
    ADD X1, X1, X0   // sum += i
    ADD X0, X0, #1   // i++
    B loop
done:
// X1 now contains the sum (55)
\`\`\`

### Countdown Loops

Counting down is sometimes more efficient because \`CBNZ\` saves a separate \`CMP\`:

\`\`\`asm
MOV X0, #5           // counter = 5
loop:
    // ... loop body ...
    SUB X0, X0, #1   // counter--
    CBNZ X0, loop    // repeat if counter != 0
\`\`\`

### Loop Building Blocks

Every loop is made from these pieces:

| Part | Instructions | Purpose |
|------|-------------|---------|
| **Init** | \`MOV\` | Set up counter/accumulator |
| **Test** | \`CMP\` + \`B.cond\` or \`CBZ\`/\`CBNZ\` | Check exit condition |
| **Body** | Any instructions | The work |
| **Update** | \`ADD\`/\`SUB\` | Advance the counter |
| **Jump back** | \`B loop\` | Return to test |

> **Tip**: When translating a high-level loop, identify these five parts first, then map each to assembly.

### Your Task

Write a program that computes the sum of integers from 1 to 10 using a loop. The answer is 55. Print it followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\t// Initialize: i=1, sum=0, N=10
\t// Loop: add i to sum, increment i
\t// When i > N, exit loop
\t// Convert sum (55) to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #1
\tMOV X1, #0
\tMOV X2, #10

loop:
\tCMP X0, X2
\tB.GT done
\tADD X1, X1, X0
\tADD X0, X0, #1
\tB loop

done:
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
      name: "prints 55",
      expected: "55\n",
    },
  ],
};
