import type { Lesson } from "../../types";

export const functionPointers: Lesson = {
  id: "function-pointers",
  title: "Function Pointers",
  chapterId: "functions",
  content: `## Indirect Branching and Function Pointers

So far, every branch target has been a fixed label known at assembly time. But what if you want to choose which function to call at runtime? This is what **indirect branching** is for.

### BR -- Branch to Register

\`BR Xn\` jumps to the address stored in register Xn:

\`\`\`asm
LDR X9, =my_label    // Load address of a code label
BR X9                 // Jump to that address
\`\`\`

Unlike \`BL\`, \`BR\` does **not** save a return address in LR. It is a plain jump, like a \`goto\`. This makes it ideal for implementing jump tables and dispatch patterns.

> Function pointers are the Changelings of assembly -- like the Founders from Deep Space Nine, they can take any form. A single register can point to any function, and you only discover which one at runtime.

### Why Indirect Branching Matters

At the hardware level, indirect branching is how the CPU implements:
- **Switch statements**: The compiler generates a jump table of addresses
- **Virtual method dispatch**: C++ vtables are arrays of function pointers
- **Callbacks**: Passing functions as arguments to other functions
- **Dynamic linking**: Shared libraries are resolved via indirect jumps

### The Jump Table Pattern

A jump table is a classic use of indirect branching. Instead of a chain of if/else branches, you compute the address of the target code and jump directly to it:

\`\`\`asm
// Each handler ends with "B done" to rejoin common code
handler_0:
    // ... handle case 0 ...
    B done
handler_1:
    // ... handle case 1 ...
    B done
handler_2:
    // ... handle case 2 ...
    B done
\`\`\`

You select a handler by loading its address and jumping:

\`\`\`asm
LDR X9, =handler_1   // select handler 1
BR X9                 // jump to it
\`\`\`

### Building a Calculator Dispatch

Here is a simple calculator pattern. Each operation computes a result and jumps to the print section:

\`\`\`asm
op_add:
    ADD X10, X0, X1
    B print_result
op_sub:
    SUB X10, X0, X1
    B print_result
op_mul:
    MUL X10, X0, X1
    B print_result

_start:
    MOV X0, #6
    MOV X1, #7
    LDR X9, =op_mul   // select multiplication
    BR X9              // dispatch!

print_result:
    // convert X10 to ASCII and print
\`\`\`

The key insight is that \`_start\` does not need to know which operation runs -- it just loads an address and jumps. Changing the dispatched function is a single-line change.

### Your Task

Write three operation blocks: \`op_add\` (computes X0 + X1), \`op_sub\` (computes X0 - X1), and \`op_mul\` (computes X0 * X1). Each should store the result in X10 and branch to \`print_result\`. In \`_start\`, load arguments 6 and 7, then dispatch to \`op_mul\` using BR. Print the result (42) followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start

op_add:
\tADD X10, X0, X1
\tB print_result

op_sub:
\tSUB X10, X0, X1
\tB print_result

op_mul:
\tMUL X10, X0, X1
\tB print_result

_start:
\t// Set X0 = 6, X1 = 7
\t// Load address of op_mul into X9
\t// Use BR X9 to dispatch
\t// (control flows to op_mul, then to print_result)

print_result:
\t// Convert X10 (42) to ASCII and print
\t// Exit
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start

op_add:
\tADD X10, X0, X1
\tB print_result

op_sub:
\tSUB X10, X0, X1
\tB print_result

op_mul:
\tMUL X10, X0, X1
\tB print_result

_start:
\tMOV X0, #6
\tMOV X1, #7
\tLDR X9, =op_mul
\tBR X9

print_result:
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
      name: "prints 42",
      expected: "42\n",
    },
  ],
};
