import type { Lesson } from "../../types";

export const recursiveFunctions: Lesson = {
  id: "recursive-functions",
  title: "Recursive Functions",
  chapterId: "functions",
  content: `## Recursion in Assembly

Recursive functions call themselves. Understanding recursion in assembly reveals exactly what the CPU does when high-level languages handle it automatically -- each recursive call creates a new stack frame, and the call stack unwinds as results are returned.

> Like Q sending Picard into nested timelines within timelines, each recursive call drops you one level deeper -- and you must unwind every layer to get back to where you started.

### The Core Problem

When a function calls itself with \`BL\`, the return address in \`X30\` is overwritten. Without saving it, the function has no way to return to its original caller. Similarly, argument registers (X0-X7) are overwritten by the recursive call's arguments.

### Stack Frame Per Call

Each recursive call pushes a "frame" onto the stack to save:
1. The link register (\`X30\`) -- so we can return
2. Any register values needed after the recursive call returns

### Factorial Example

Here is \`factorial(n) = n!\` implemented recursively:

\`\`\`asm
factorial:
    CMP X0, #1
    B.LE base_case         // if n <= 1, return 1

    STP X30, X0, [SP, #-16]!  // Save LR and n

    SUB X0, X0, #1         // Argument: n-1
    BL factorial            // Recursive call: result in X0

    LDP X30, X1, [SP], #16 // Restore LR and n (into X1)

    MUL X0, X0, X1         // Return n * factorial(n-1)
    RET

base_case:
    MOV X0, #1
    RET
\`\`\`

### Stack Walkthrough for factorial(5)

\`\`\`
Call factorial(5): push (LR, 5)  → stack: [(LR,5)]
Call factorial(4): push (LR, 4)  → stack: [(LR,5), (LR,4)]
Call factorial(3): push (LR, 3)  → stack: [(LR,5), (LR,4), (LR,3)]
Call factorial(2): push (LR, 2)  → stack: [(LR,5), (LR,4), (LR,3), (LR,2)]
Call factorial(1): base case!    → return 1

Pop (LR, 2): return 2 * 1 = 2
Pop (LR, 3): return 3 * 2 = 6
Pop (LR, 4): return 4 * 6 = 24
Pop (LR, 5): return 5 * 24 = 120
\`\`\`

Each frame is exactly 16 bytes (two 8-byte registers), so factorial(5) uses 64 bytes of stack at peak depth.

> **Common mistake**: Forgetting to save X0 (the current n) before the recursive call. After \`BL factorial\` returns, X0 holds the result of the recursive call, not the original n. You need to have saved n somewhere (e.g., on the stack) to use it in the multiplication.

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
