import type { Lesson } from "../../types";

export const functionArgs: Lesson = {
  id: "function-args",
  title: "Function Arguments",
  chapterId: "functions",
  content: `## The System V AMD64 Calling Convention

On Linux x86_64, the standard calling convention passes function arguments in registers:

| Argument | Register |
|----------|----------|
| 1st | \`rdi\` |
| 2nd | \`rsi\` |
| 3rd | \`rdx\` |
| 4th | \`rcx\` |
| 5th | \`r8\` |
| 6th | \`r9\` |
| Return value | \`rax\` |

### Stack Frame

Functions that call other functions should set up a stack frame:

\`\`\`asm
my_function:
\tpush rbp           ; save old base pointer
\tmov rbp, rsp       ; set up new frame
\t; ... function body ...
\tpop rbp            ; restore base pointer
\tret
\`\`\`

### Callee-Saved Registers

The callee must preserve: \`rbx\`, \`rbp\`, \`r12\`-\`r15\`. All other registers may be clobbered.

### Your Task

Write a function \`add_three\` that:
- Takes three arguments in \`rdi\`, \`rsi\`, \`rdx\`
- Returns their sum in \`rax\`

Call it with (2, 3, 4) and print the result (9), then call it with (1, 5, 1) and print the result (7).

Print: "9\\n7\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Call add_three(2, 3, 4)
\tmov rdi, 2
\tmov rsi, 3
\tmov rdx, 4
\tcall add_three
\t; rax should be 9

\t; Print result
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall
\tadd rsp, 8

\t; Call add_three(1, 5, 1)
\tmov rdi, 1
\tmov rsi, 5
\tmov rdx, 1
\tcall add_three

\t; Print result
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall
\tadd rsp, 8

\tmov rax, 60
\tmov rdi, 0
\tsyscall

; add_three(rdi, rsi, rdx) -> rax
add_three:
\t; YOUR CODE HERE
\tret
`,

  solution: `section .text
global _start
_start:
\tmov rdi, 2
\tmov rsi, 3
\tmov rdx, 4
\tcall add_three

\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall
\tadd rsp, 8

\tmov rdi, 1
\tmov rsi, 5
\tmov rdx, 1
\tcall add_three

\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall
\tadd rsp, 8

\tmov rax, 60
\tmov rdi, 0
\tsyscall

add_three:
\tmov rax, rdi
\tadd rax, rsi
\tadd rax, rdx
\tret
`,

  tests: [
    {
      name: "prints 9 and 7",
      expected: "9\n7\n",
    },
  ],
};
