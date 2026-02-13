import type { Lesson } from "../../types";

export const functionsAndBl: Lesson = {
  id: "functions-and-bl",
  title: "Functions and BL",
  chapterId: "functions",
  content: `## Functions in ARM64

ARM64 implements function calls using two key concepts: the \`BL\` instruction and the link register (\`LR\`/\`X30\`).

### BL -- Branch with Link

\`BL\` branches to a label and saves the return address in \`X30\` (the Link Register):

\`\`\`asm
BL my_function       // Jump to my_function, save return address in X30
\`\`\`

### RET -- Return

\`RET\` branches to the address in \`X30\`, returning from the function:

\`\`\`asm
my_function:
    // ... function body ...
    RET              // Return to caller (jump to address in X30)
\`\`\`

### A Simple Function

Here is a function that doubles a value in \`X0\`:

\`\`\`asm
double:
    ADD X0, X0, X0   // X0 = X0 + X0
    RET

_start:
    MOV X0, #21
    BL double         // Call double, X0 = 42 on return
\`\`\`

### Function Arguments

By convention, function arguments are passed in \`X0\` through \`X7\`, and the return value is in \`X0\`:

\`\`\`asm
// add(a, b) -- returns a + b
add:
    ADD X0, X0, X1   // X0 = X0 (a) + X1 (b)
    RET

_start:
    MOV X0, #3       // first argument
    MOV X1, #4       // second argument
    BL add            // X0 = 7 on return
\`\`\`

### Your Task

Write a function called \`square\` that takes a number in \`X0\` and returns its square (X0 * X0) in \`X0\`. Call it from \`_start\` with the value 7, then print the result (49) followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start

// Define the square function here
// It should compute X0 = X0 * X0 and return

_start:
\tMOV X0, #7
\t// Call square
\t// Convert result to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start

square:
\tMUL X0, X0, X0
\tRET

_start:
\tMOV X0, #7
\tBL square

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
      name: "prints 49",
      expected: "49\n",
    },
  ],
};
