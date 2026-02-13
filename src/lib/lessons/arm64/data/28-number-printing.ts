import type { Lesson } from "../../types";

export const numberPrinting: Lesson = {
  id: "number-printing",
  title: "Number Printing",
  chapterId: "challenges",
  content: `## Printing Multi-Digit Numbers

Until now, we've only printed single-digit numbers by adding 48 (\`'0'\`). But what about larger numbers like 2048? We need to convert each digit to its ASCII character.

### The Divide-by-10 Algorithm

To extract digits from a number, repeatedly divide by 10. The remainder gives the last digit, and the quotient gives the remaining number:

\`\`\`
2048 / 10 = 204 remainder 8
 204 / 10 = 20  remainder 4
  20 / 10 = 2   remainder 0
   2 / 10 = 0   remainder 2
\`\`\`

The digits come out in **reverse order** (8, 4, 0, 2), so we fill a buffer from right to left.

### Computing a Remainder

ARM64 has no modulo operator, but remainder = \`n - (n/10) * 10\`:

\`\`\`asm
MOV X1, #10
UDIV X2, X0, X1        // quotient = n / 10
MUL X3, X2, X1         // quotient * 10
SUB X3, X0, X3         // remainder = n - quotient * 10
ADD X3, X3, #48        // convert to ASCII digit
\`\`\`

### Filling from Right to Left

Start at the end of the buffer. Store each digit and move the pointer backward:

\`\`\`asm
LDR X5, =buf
ADD X5, X5, #10       // Start near end

digit_loop:
    // ... compute remainder digit ...
    STRB W3, [X5]     // Store digit
    SUB X5, X5, #1    // Move left
    MOV X0, X2        // n = quotient
    CBNZ X0, digit_loop
\`\`\`

After the loop, \`X5 + 1\` points to the first digit.

### Your Task

Print the number \`2048\` followed by a newline. Use the divide-by-10 approach to convert the number to its ASCII digits, store them in a buffer, then print.`,

  starterCode: `.data
buf:
\t.skip 12

.text
.global _start
_start:
\tMOV X0, #2048

\t// Fill buffer from right to left:
\t// Store newline at end
\t// Loop: divide by 10, store remainder digit, move left
\t// Print from first digit to newline
`,

  solution: `.data
buf:
\t.skip 12

.text
.global _start
_start:
\tMOV X0, #2048

\tLDR X5, =buf
\tADD X5, X5, #10
\tMOV X6, X5

\tMOV W7, #10
\tSTRB W7, [X5]
\tSUB X5, X5, #1

digit_loop:
\tMOV X1, #10
\tUDIV X2, X0, X1
\tMUL X3, X2, X1
\tSUB X3, X0, X3
\tADD X3, X3, #48
\tSTRB W3, [X5]
\tSUB X5, X5, #1
\tMOV X0, X2
\tCBNZ X0, digit_loop

\tADD X5, X5, #1
\tADD X6, X6, #1
\tSUB X2, X6, X5

\tMOV X0, #1
\tMOV X1, X5
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints 2048",
      expected: "2048\n",
    },
  ],
};
