import type { Lesson } from "../../types";

export const stringReversal: Lesson = {
  id: "string-reversal",
  title: "String Reversal",
  chapterId: "challenges",
  content: `## Reversing a String

Reversing a string in-place is a classic exercise that combines string iteration, memory access, and the two-pointer technique.

### The Two-Pointer Technique

Use two indices: one starting at the beginning (\`left\`) and one at the end (\`right\`). Swap the characters at these positions, then move both pointers inward:

\`\`\`
"abcde"
 ^   ^    swap 'a' and 'e' → "ebcda"
  ^ ^     swap 'b' and 'd' → "edcba"
  ^^       left >= right, done!
\`\`\`

### Step 1: Find the Length

First, iterate to find the string length (position of the null terminator):

\`\`\`asm
LDR X0, =str
MOV X1, #0
len_loop:
    LDRB W2, [X0, X1]
    CBZ W2, len_done
    ADD X1, X1, #1
    B len_loop
len_done:
// X1 = length
\`\`\`

### Step 2: Swap with Two Pointers

\`\`\`asm
MOV X2, #0           // left = 0
SUB X3, X1, #1       // right = length - 1

rev_loop:
    CMP X2, X3
    B.GE rev_done
    LDRB W4, [X0, X2]   // tmp = str[left]
    LDRB W5, [X0, X3]   // load str[right]
    STRB W5, [X0, X2]   // str[left] = str[right]
    STRB W4, [X0, X3]   // str[right] = tmp
    ADD X2, X2, #1
    SUB X3, X3, #1
    B rev_loop
rev_done:
\`\`\`

### Your Task

Reverse the string \`"abcde"\` in-place and print the result (\`edcba\`) followed by a newline.`,

  starterCode: `.data
str:
\t.asciz "abcde"
buf:
\t.skip 6

.text
.global _start
_start:
\t// Step 1: Find the length of str
\t// Step 2: Use two pointers to reverse in-place
\t// Step 3: Copy to buf, add newline, print
`,

  solution: `.data
str:
\t.asciz "abcde"
buf:
\t.skip 6

.text
.global _start
_start:
\tLDR X0, =str
\tMOV X1, #0

len_loop:
\tLDRB W2, [X0, X1]
\tCBZ W2, len_done
\tADD X1, X1, #1
\tB len_loop

len_done:
\tMOV X2, #0
\tSUB X3, X1, #1

rev_loop:
\tCMP X2, X3
\tB.GE rev_done
\tLDRB W4, [X0, X2]
\tLDRB W5, [X0, X3]
\tSTRB W5, [X0, X2]
\tSTRB W4, [X0, X3]
\tADD X2, X2, #1
\tSUB X3, X3, #1
\tB rev_loop

rev_done:
\tLDR X6, =buf
\tMOV X7, #0
copy_loop:
\tLDRB W8, [X0, X7]
\tCBZ W8, copy_done
\tSTRB W8, [X6, X7]
\tADD X7, X7, #1
\tB copy_loop

copy_done:
\tMOV W9, #10
\tSTRB W9, [X6, X7]
\tADD X7, X7, #1

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, X7
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "reverses abcde to edcba",
      expected: "edcba\n",
    },
  ],
};
