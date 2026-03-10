import type { Lesson } from "../../types";

export const dataSection: Lesson = {
  id: "data-section",
  title: "The Data Section",
  chapterId: "basics",
  content: `## The Data Section

The \`.data\` section holds initialized data that your program uses. You define variables using data directives.

### Data Directives

| Directive | Size | Example |
|-----------|------|---------|
| \`db\` | 1 byte | \`db 0x41\` or \`db "A"\` |
| \`dw\` | 2 bytes (word) | \`dw 1000\` |
| \`dd\` | 4 bytes (double word) | \`dd 100000\` |
| \`dq\` | 8 bytes (quad word) | \`dq 0xFFFFFFFF\` |

### Strings

Strings are just sequences of bytes. Common patterns:

\`\`\`asm
msg db "Hello", 10     ; String followed by newline (10 = '\\n')
msg db "Hi", 0         ; Null-terminated string
msg db 72, 101, 108    ; Individual byte values
\`\`\`

The value \`10\` is the ASCII code for a newline character. You append it after the string with a comma.

### Multiple Data Items

You can define multiple items:

\`\`\`asm
section .data
greeting db "Hello", 10
farewell db "Bye", 10
number dd 42
\`\`\`

Each label points to the start of its data in memory.

### Your Task

Define three strings in the data section and print them in order:
1. "One" followed by a newline (4 bytes)
2. "Two" followed by a newline (4 bytes)
3. "Three" followed by a newline (6 bytes)`,

  starterCode: `section .data
; Define your three strings here
; YOUR CODE HERE

section .text
global _start
_start:
\t; Print all three strings
\t; YOUR CODE HERE

\t; Exit
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .data
one db "One", 10
two db "Two", 10
three db "Three", 10

section .text
global _start
_start:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [one]
\tmov rdx, 4
\tsyscall

\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [two]
\tmov rdx, 4
\tsyscall

\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [three]
\tmov rdx, 6
\tsyscall

\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints One Two Three",
      expected: "One\nTwo\nThree\n",
    },
  ],
};
