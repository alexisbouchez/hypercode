import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## Loops in Assembly

ARM64 has no dedicated loop instruction. Loops are built from branches and comparisons.

### A Counting Loop

A typical for-loop pattern (i = 0; i < N; i++):

\`\`\`asm
MOV X0, #0           // i = 0
MOV X1, #10          // N = 10
loop:
    CMP X0, X1
    B.GE done        // if i >= N, exit loop
    // ... loop body ...
    ADD X0, X0, #1   // i++
    B loop           // repeat
done:
\`\`\`

### A Sum Loop

Adding numbers 1 through N:

\`\`\`asm
MOV X0, #1           // i = 1
MOV X1, #0           // sum = 0
MOV X2, #10          // N = 10
loop:
    CMP X0, X2
    B.GT done
    ADD X1, X1, X0   // sum += i
    ADD X0, X0, #1   // i++
    B loop
done:
// X1 now contains the sum
\`\`\`

### Using CBZ/CBNZ for Loops

You can also loop using \`CBNZ\` (branch if not zero) for countdown loops:

\`\`\`asm
MOV X0, #5           // counter = 5
loop:
    // ... loop body ...
    SUB X0, X0, #1   // counter--
    CBNZ X0, loop    // repeat if counter != 0
\`\`\`

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
