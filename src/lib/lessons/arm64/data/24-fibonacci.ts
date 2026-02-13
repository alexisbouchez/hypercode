import type { Lesson } from "../../types";

export const fibonacci: Lesson = {
  id: "fibonacci",
  title: "Fibonacci Sequence",
  chapterId: "challenges",
  content: `## Fibonacci Sequence

The Fibonacci sequence is defined as:
\`\`\`
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
\`\`\`

The first few values: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, **55**, ...

### Iterative Approach

While recursion is elegant, the iterative approach is much more efficient -- it uses just two registers instead of a deep call stack:

\`\`\`asm
MOV X0, #0          // prev = fib(0)
MOV X1, #1          // curr = fib(1)
MOV X2, #10         // n = 10
MOV X3, #0          // i = 0

fib_loop:
    CMP X3, X2
    B.GE fib_done
    ADD X4, X0, X1  // next = prev + curr
    MOV X0, X1      // prev = curr
    MOV X1, X4      // curr = next
    ADD X3, X3, #1
    B fib_loop

fib_done:
// X0 = fib(10) = 55
\`\`\`

### Register Allocation

This is a great exercise in register management:
- \`X0\` = previous value
- \`X1\` = current value
- \`X2\` = target n
- \`X3\` = loop counter
- \`X4\` = temporary for the sum

### Printing the Result

Since \`fib(10) = 55\` is a two-digit number, you'll need to convert it to ASCII. Divide by 10 to get each digit:

\`\`\`asm
MOV X5, #10
UDIV X6, X0, X5        // tens digit
MUL X7, X6, X5
SUB X7, X0, X7         // ones digit
ADD X6, X6, #48        // to ASCII
ADD X7, X7, #48        // to ASCII
\`\`\`

### Your Task

Compute \`fib(10)\` iteratively and print the result (\`55\`) followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\t// Initialize prev=0, curr=1, n=10
\t// Loop: next = prev + curr, shift values
\t// After loop, X0 = fib(10) = 55
\t// Convert to ASCII digits and print
`,

  solution: `.data
buf:
\t.skip 3

.text
.global _start
_start:
\tMOV X0, #0
\tMOV X1, #1
\tMOV X2, #10
\tMOV X3, #0

fib_loop:
\tCMP X3, X2
\tB.GE fib_done
\tADD X4, X0, X1
\tMOV X0, X1
\tMOV X1, X4
\tADD X3, X3, #1
\tB fib_loop

fib_done:
\tMOV X5, #10
\tUDIV X6, X0, X5
\tMUL X7, X6, X5
\tSUB X7, X0, X7
\tADD X6, X6, #48
\tADD X7, X7, #48

\tLDR X9, =buf
\tSTRB W6, [X9]
\tSTRB W7, [X9, #1]
\tMOV W10, #10
\tSTRB W10, [X9, #2]

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
      name: "prints fib(10) = 55",
      expected: "55\n",
    },
  ],
};
