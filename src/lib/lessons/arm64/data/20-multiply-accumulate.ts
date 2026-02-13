import type { Lesson } from "../../types";

export const multiplyAccumulate: Lesson = {
  id: "multiply-accumulate",
  title: "Multiply-Accumulate",
  chapterId: "putting-it-together",
  content: `## MADD and MSUB: Fused Multiply-Accumulate

Many real-world computations follow the pattern \`a + (b * c)\` or \`a - (b * c)\`. ARM64 provides dedicated instructions for these patterns that execute in a single cycle.

> "Warp 9.975, Mr. La Forge!" -- computing warp factors requires some serious multiply-accumulate math. Fortunately, MADD does it in a single instruction.

### MADD -- Multiply-Add

\`MADD\` computes \`Xd = Xa + (Xn * Xm)\`:

\`\`\`asm
MADD X0, X1, X2, X3   // X0 = X3 + (X1 * X2)
\`\`\`

The order of operands is: destination, first multiply source, second multiply source, addend.

A common use: computing \`MUL\` is actually encoded as \`MADD Xd, Xn, Xm, XZR\` (multiply and add zero).

### MSUB -- Multiply-Subtract

\`MSUB\` computes \`Xd = Xa - (Xn * Xm)\`:

\`\`\`asm
MSUB X0, X1, X2, X3   // X0 = X3 - (X1 * X2)
\`\`\`

This is extremely useful for computing remainders. Remember the remainder pattern?

\`\`\`asm
// Old way: three instructions
UDIV X2, X0, X1       // quotient = a / b
MUL  X3, X2, X1       // temp = quotient * b
SUB  X4, X0, X3       // remainder = a - temp

// New way: two instructions with MSUB
UDIV X2, X0, X1       // quotient = a / b
MSUB X4, X2, X1, X0   // remainder = a - (quotient * b)
\`\`\`

### Dot Product

The **dot product** of two vectors is a fundamental operation in linear algebra, graphics, machine learning, and signal processing. For two vectors A and B of length N:

\`\`\`
dot = A[0]*B[0] + A[1]*B[1] + ... + A[N-1]*B[N-1]
\`\`\`

MADD is perfect for this. Start with an accumulator of 0 and repeatedly multiply-add:

\`\`\`asm
MOV X4, #0             // accumulator = 0
// For each pair (a, b):
MADD X4, Xa, Xb, X4   // accumulator += a * b
\`\`\`

### Polynomial Evaluation

Another common application is evaluating polynomials. For \`ax^2 + bx + c\`, you can use Horner's method: \`(a*x + b)*x + c\`:

\`\`\`asm
// Evaluate 3x^2 + 2x + 1 at x=5
MOV X0, #5         // x
MOV X1, #3         // a
MOV X2, #2         // b
MOV X3, #1         // c
MADD X1, X1, X0, X2  // X1 = a*x + b = 17
MADD X1, X1, X0, X3  // X1 = (a*x + b)*x + c = 86
\`\`\`

### Your Task

Compute the dot product of two 4-element vectors:
- A = [3, 5, 2, 4]
- B = [1, 4, 6, 2]

The dot product is: 3*1 + 5*4 + 2*6 + 4*2 = 3 + 20 + 12 + 8 = 43.

Use \`MADD\` for the accumulation. Print the result (43) followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\t// Vector A: 3, 5, 2, 4
\t// Vector B: 1, 4, 6, 2
\t// Compute dot product using MADD
\t// Start with accumulator = 0
\t// For each pair, use MADD to multiply-add
\t// Convert result (43) to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X10, #0

\tMOV X0, #3
\tMOV X1, #1
\tMADD X10, X0, X1, X10

\tMOV X0, #5
\tMOV X1, #4
\tMADD X10, X0, X1, X10

\tMOV X0, #2
\tMOV X1, #6
\tMADD X10, X0, X1, X10

\tMOV X0, #4
\tMOV X1, #2
\tMADD X10, X0, X1, X10

\tMOV X3, #10
\tUDIV X4, X10, X3
\tMUL X5, X4, X3
\tSUB X5, X10, X5

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
      name: "prints 43",
      expected: "43\n",
    },
  ],
};
