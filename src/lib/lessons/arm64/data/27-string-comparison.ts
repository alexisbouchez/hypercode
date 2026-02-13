import type { Lesson } from "../../types";

export const stringComparison: Lesson = {
  id: "string-comparison",
  title: "String Comparison",
  chapterId: "challenges",
  content: `## Comparing Strings

Comparing two strings byte-by-byte is one of the most common operations in programming. In assembly, you implement it manually -- there is no built-in \`strcmp\`.

### The Algorithm

Compare corresponding bytes from two strings until:
1. You find two bytes that differ → strings are **not equal**
2. Both bytes are null terminators → strings are **equal**

\`\`\`asm
LDR X0, =str1
LDR X1, =str2

cmp_loop:
    LDRB W2, [X0], #1     // Load byte from str1, advance
    LDRB W3, [X1], #1     // Load byte from str2, advance
    CMP W2, W3
    B.NE not_equal         // Bytes differ
    CBZ W2, equal          // Both are null → match!
    B cmp_loop

equal:
    // Strings are equal
not_equal:
    // Strings are different
\`\`\`

### Post-Increment for Iteration

Notice \`[X0], #1\` -- this is the post-index addressing mode. It loads the byte at \`[X0]\`, **then** adds 1 to X0. Perfect for walking through strings.

### Why Check Both Conditions?

- \`CMP W2, W3\` / \`B.NE\` catches mismatched bytes at any position
- \`CBZ W2\` catches the end of both strings (if we reach here, W2 == W3, so if W2 is 0 then both strings ended at the same point)

### Your Task

Compare the strings \`"hello"\` and \`"hello"\`. If they are equal, print \`EQ\` followed by a newline. If not, print \`NE\` followed by a newline.`,

  starterCode: `.data
str1:
\t.asciz "hello"
str2:
\t.asciz "hello"
eq_msg:
\t.ascii "EQ\\n"
ne_msg:
\t.ascii "NE\\n"

.text
.global _start
_start:
\t// Load addresses of str1 and str2
\t// Compare byte by byte
\t// If equal: print eq_msg (3 bytes)
\t// If not equal: print ne_msg (3 bytes)
\t// Exit
`,

  solution: `.data
str1:
\t.asciz "hello"
str2:
\t.asciz "hello"
eq_msg:
\t.ascii "EQ\\n"
ne_msg:
\t.ascii "NE\\n"

.text
.global _start
_start:
\tLDR X0, =str1
\tLDR X1, =str2

cmp_loop:
\tLDRB W2, [X0], #1
\tLDRB W3, [X1], #1
\tCMP W2, W3
\tB.NE not_equal
\tCBZ W2, equal
\tB cmp_loop

equal:
\tMOV X0, #1
\tLDR X1, =eq_msg
\tMOV X2, #3
\tMOV X8, #64
\tSVC #0
\tB done

not_equal:
\tMOV X0, #1
\tLDR X1, =ne_msg
\tMOV X2, #3
\tMOV X8, #64
\tSVC #0

done:
\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "equal strings print EQ",
      expected: "EQ\n",
    },
  ],
};
