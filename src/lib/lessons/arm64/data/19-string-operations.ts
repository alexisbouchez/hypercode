import type { Lesson } from "../../types";

export const stringOperations: Lesson = {
  id: "string-operations",
  title: "String Operations",
  chapterId: "putting-it-together",
  content: `## Working with Strings

Strings in assembly are just sequences of bytes in memory. There is no \`String\` type, no \`.length\` property, no built-in functions -- you process them one byte at a time using \`LDRB\` and \`STRB\`. This lesson brings together loops, memory operations, comparisons, and bitwise ops.

> Processing strings byte by byte is like the universal translator on the Enterprise -- decoding alien languages one character at a time until the message becomes clear.

### Null-Terminated Strings

In C and assembly, strings end with a **null terminator** -- a byte with value 0. The \`.asciz\` directive adds this automatically:

\`\`\`asm
msg:  .asciz "hello"   // Stores: 'h','e','l','l','o', 0  (6 bytes)
msg2: .ascii "hello"   // Stores: 'h','e','l','l','o'     (5 bytes, no terminator)
\`\`\`

### Iterating Over a String

Loop until you hit the null terminator:

\`\`\`asm
LDR X0, =myStr
loop:
    LDRB W1, [X0], #1    // Load byte, advance pointer
    CBZ W1, done          // Stop at null terminator (0)
    // ... process byte in W1 ...
    B loop
done:
\`\`\`

### String Length

Count characters until the null terminator:

\`\`\`asm
LDR X0, =myStr
MOV X1, #0            // length = 0
len_loop:
    LDRB W2, [X0], #1
    CBZ W2, len_done
    ADD X1, X1, #1
    B len_loop
len_done:
// X1 = length (not counting the null terminator)
\`\`\`

### ASCII Case Conversion

ASCII was cleverly designed so that uppercase and lowercase letters differ by exactly one bit:

| Character | ASCII | Binary |
|-----------|-------|--------|
| 'A' | 65 | 0**1**000001 |
| 'a' | 97 | 0**1**100001 |

The difference is bit 5 (value 32). To convert case:
- **To lowercase**: \`ORR W0, W0, #0x20\` (set bit 5)
- **To uppercase**: \`SUB W0, W0, #32\` or \`AND W0, W0, #0xDF\` (clear bit 5)

But you must check the character is actually a letter first:

\`\`\`asm
CMP W1, #97          // 'a'
B.LT skip            // Not a lowercase letter
CMP W1, #122         // 'z'
B.GT skip            // Not a lowercase letter
SUB W1, W1, #32      // Convert to uppercase
skip:
\`\`\`

> **Common mistake**: Applying case conversion to spaces, digits, or punctuation. Always check the range first.

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
