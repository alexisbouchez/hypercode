import type { Lesson } from "../../types";

export const stringOperations: Lesson = {
  id: "string-operations",
  title: "String Operations",
  chapterId: "putting-it-together",
  content: `## Working with Strings

Strings in assembly are just sequences of bytes in memory. You process them one byte at a time using \`LDRB\` and \`STRB\`.

### Iterating Over a String

A null-terminated string (\`.asciz\`) ends with a zero byte. Loop until you find it:

\`\`\`asm
LDR X0, =myStr
loop:
    LDRB W1, [X0], #1    // Load byte, advance pointer
    CBZ W1, done          // Stop at null terminator
    // ... process byte in W1 ...
    B loop
done:
\`\`\`

### String Length

Count characters until the null terminator:

\`\`\`asm
LDR X0, =myStr
MOV X1, #0
len_loop:
    LDRB W2, [X0], #1
    CBZ W2, len_done
    ADD X1, X1, #1
    B len_loop
len_done:
// X1 = length
\`\`\`

### Case Conversion

ASCII uppercase letters are 65-90 ('A'-'Z'), lowercase are 97-122 ('a'-'z'). The difference is 32 (bit 5):

- To lowercase: \`ORR W0, W0, #0x20\` (set bit 5)
- To uppercase: \`AND W0, W0, #0xDF\` (clear bit 5)

But you should only convert actual letters, not spaces or punctuation.

\`\`\`asm
// Convert to uppercase if it's a lowercase letter
CMP W1, #97          // 'a'
B.LT skip
CMP W1, #122         // 'z'
B.GT skip
AND W1, W1, #0xDF    // Clear bit 5 -> uppercase
skip:
\`\`\`

### Your Task

Write a program that converts the string \`"hello"\` to uppercase and prints \`HELLO\\n\`.

Hint: Iterate over each byte, check if it is a lowercase letter (97-122), convert if so, store the result, then print.`,

  starterCode: `.data
msg:
\t.asciz "hello"
buf:
\t.skip 6

.text
.global _start
_start:
\t// Load address of msg and buf
\t// Loop over each character in msg
\t// If lowercase letter, convert to uppercase
\t// Store in buf
\t// After loop, add newline to buf
\t// Print buf and exit
`,

  solution: `.data
msg:
\t.asciz "hello"
buf:
\t.skip 6

.text
.global _start
_start:
\tLDR X0, =msg
\tLDR X3, =buf
\tMOV X4, #0

to_upper_loop:
\tLDRB W1, [X0], #1
\tCBZ W1, to_upper_done
\tCMP W1, #97
\tB.LT store_char
\tCMP W1, #122
\tB.GT store_char
\tSUB W1, W1, #32

store_char:
\tSTRB W1, [X3, X4]
\tADD X4, X4, #1
\tB to_upper_loop

to_upper_done:
\tMOV W5, #10
\tSTRB W5, [X3, X4]
\tADD X4, X4, #1

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, X4
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints HELLO",
      expected: "HELLO\n",
    },
  ],
};
