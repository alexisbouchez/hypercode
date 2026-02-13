import type { Lesson } from "../../types";

export const callingConvention: Lesson = {
  id: "calling-convention",
  title: "Calling Convention",
  chapterId: "functions",
  content: `## The AArch64 Calling Convention

The calling convention is a contract between functions: it specifies which registers hold arguments, where return values go, and who is responsible for saving each register. Following it is essential when your code interacts with other functions.

> Like Starfleet protocol, the calling convention is a standardized set of procedures so that every function -- like every crew member -- knows the rules of engagement.

### Register Roles

| Registers | Role | Saved by |
|-----------|------|----------|
| \`X0\`-\`X7\` | Arguments and return values | Caller |
| \`X8\` | Indirect result / syscall number | Caller |
| \`X9\`-\`X15\` | Temporary (scratch) | Caller |
| \`X19\`-\`X28\` | Callee-saved | Callee |
| \`X29\` (\`FP\`) | Frame pointer | Callee |
| \`X30\` (\`LR\`) | Link register | Callee |

### Caller-Saved vs Callee-Saved

This distinction is the most important concept:

- **Caller-saved** (X0-X15): The called function may destroy these freely. If you need a value in X0-X15 to survive a \`BL\`, you must save it yourself before the call.
- **Callee-saved** (X19-X28, FP, LR): The called function **promises** to restore these before returning. If a function wants to use X19-X28, it must push them to the stack first and restore them at the end.

Think of it this way: caller-saved registers are "your problem", callee-saved registers are "their problem."

### Saving LR When Calling Functions

If a function calls another function, it must save \`LR\` (X30) because the inner \`BL\` will overwrite it:

\`\`\`asm
outer:
    STP X29, X30, [SP, #-16]!  // Save FP and LR
    MOV X0, #5
    BL inner                     // This overwrites X30!
    // X30 now points to inner's return, not outer's return
    LDP X29, X30, [SP], #16    // Restore FP and LR
    RET                          // Now RET correctly returns to outer's caller

inner:
    ADD X0, X0, #1
    RET
\`\`\`

> **Rule of thumb**: Any function that uses \`BL\` must save/restore X30. Leaf functions (functions that do not call other functions) can skip this since their X30 is never overwritten.

### Argument Passing

Arguments go in X0-X7 in order. Return values come back in X0:

\`\`\`asm
// max(a, b) -- returns the larger value
max:
    CMP X0, X1
    B.GE max_done
    MOV X0, X1        // Return larger value in X0
max_done:
    RET
\`\`\`

### Your Task

Write two functions:
1. \`add_three\` -- takes three arguments in X0, X1, X2 and returns their sum in X0
2. \`_start\` -- calls \`add_three\` with arguments 11, 22, and 33, then prints the result (66) followed by a newline`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start

// Define add_three: returns X0 + X1 + X2 in X0

_start:
\t// Load arguments 11, 22, 33
\t// Call add_three
\t// Convert result to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start

add_three:
\tADD X0, X0, X1
\tADD X0, X0, X2
\tRET

_start:
\tMOV X0, #11
\tMOV X1, #22
\tMOV X2, #33
\tBL add_three

\tMOV X9, X0
\tMOV X1, #10
\tUDIV X2, X9, X1
\tMUL X3, X2, X1
\tSUB X3, X9, X3

\tADD X2, X2, #48
\tADD X3, X3, #48

\tLDR X4, =buf
\tSTRB W2, [X4]
\tSTRB W3, [X4, #1]
\tMOV X5, #10
\tSTRB W5, [X4, #2]

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
      name: "prints 66",
      expected: "66\n",
    },
  ],
};
