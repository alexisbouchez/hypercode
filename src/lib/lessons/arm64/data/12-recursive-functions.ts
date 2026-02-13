import type { Lesson } from "../../types";

export const recursiveFunctions: Lesson = {
  id: "recursive-functions",
  title: "Recursive Functions",
  chapterId: "functions",
  content: `## Recursion in Assembly

Recursive functions call themselves. The key challenge is saving and restoring the link register (\`LR\`) and any values needed across the recursive call.

### The Problem

When a function calls itself with \`BL\`, the return address in \`X30\` is overwritten. Without saving it, the function cannot return to its original caller.

### Stack Frames

Each recursive call pushes a "frame" onto the stack to save:
1. The link register (\`X30\`)
2. Any register values needed after the recursive call

### Factorial Example

Here is factorial(n) = n! recursively:

\`\`\`asm
factorial:
    // Base case: if n <= 1, return 1
    CMP X0, #1
    B.LE base_case

    // Save LR and n on the stack
    STP X30, X0, [SP, #-16]!

    // Recursive call: factorial(n-1)
    SUB X0, X0, #1
    BL factorial

    // Restore n and LR
    LDP X30, X1, [SP], #16

    // Return n * factorial(n-1)
    MUL X0, X0, X1
    RET

base_case:
    MOV X0, #1
    RET
\`\`\`

### How It Works

For \`factorial(5)\`:
1. Push LR, X0=5. Call factorial(4).
2. Push LR, X0=4. Call factorial(3).
3. Push LR, X0=3. Call factorial(2).
4. Push LR, X0=2. Call factorial(1).
5. Base case: return 1.
6. Pop X0=2, return 2*1=2.
7. Pop X0=3, return 3*2=6.
8. Pop X0=4, return 4*6=24.
9. Pop X0=5, return 5*24=120.

### Your Task

Implement the \`factorial\` function above and call it with n=5. Print the result (120) followed by a newline.

Hint: 120 is three digits. You need to handle hundreds, tens, and ones digits.`,

  starterCode: `.data
buf:
\t.skip 4

.text
.global _start

// Define factorial function here

_start:
\tMOV X0, #5
\t// Call factorial
\t// Convert result (120) to ASCII digits and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 4

.text
.global _start

factorial:
\tCMP X0, #1
\tB.LE base_case
\tSTP X30, X0, [SP, #-16]!
\tSUB X0, X0, #1
\tBL factorial
\tLDP X30, X1, [SP], #16
\tMUL X0, X0, X1
\tRET

base_case:
\tMOV X0, #1
\tRET

_start:
\tMOV X0, #5
\tBL factorial

\tMOV X9, X0
\tMOV X10, #100
\tUDIV X11, X9, X10
\tMUL X12, X11, X10
\tSUB X9, X9, X12

\tMOV X10, #10
\tUDIV X12, X9, X10
\tMUL X13, X12, X10
\tSUB X13, X9, X13

\tADD X11, X11, #48
\tADD X12, X12, #48
\tADD X13, X13, #48

\tLDR X4, =buf
\tSTRB W11, [X4]
\tSTRB W12, [X4, #1]
\tSTRB W13, [X4, #2]
\tMOV X5, #10
\tSTRB W5, [X4, #3]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #4
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints 120",
      expected: "120\n",
    },
  ],
};
