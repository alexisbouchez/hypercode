import type { Lesson } from "../../types";

export const callRet: Lesson = {
  id: "call-ret",
  title: "Call & Return",
  chapterId: "functions",
  content: `## Functions with CALL and RET

Functions in assembly are created with \`call\` and \`ret\`:

\`\`\`asm
call my_function   ; push return address, jump to my_function
; ... execution continues here after ret ...

my_function:
\t; function body
\tret                ; pop return address, jump back to caller
\`\`\`

### How It Works

1. \`call label\` pushes the address of the next instruction onto the stack, then jumps to \`label\`
2. \`ret\` pops the return address from the stack and jumps to it

### A Simple Function

\`\`\`asm
; Function that prints a character stored in rbx
print_char:
\tpush rbx
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8
\tret
\`\`\`

### Your Task

Write a \`print_newline\` function and a \`print_digit\` function:
- \`print_digit\`: takes a digit (0-9) in \`rbx\`, converts to ASCII, and prints it
- \`print_newline\`: prints a newline character

Use these functions to print "3\\n7\\n"`,

  starterCode: `section .text
global _start
_start:
\tmov rbx, 3
\tcall print_digit
\tcall print_newline

\tmov rbx, 7
\tcall print_digit
\tcall print_newline

\tmov rax, 60
\tmov rdi, 0
\tsyscall

; Print the digit in rbx (0-9)
print_digit:
\t; YOUR CODE HERE
\tret

; Print a newline
print_newline:
\t; YOUR CODE HERE
\tret
`,

  solution: `section .text
global _start
_start:
\tmov rbx, 3
\tcall print_digit
\tcall print_newline

\tmov rbx, 7
\tcall print_digit
\tcall print_newline

\tmov rax, 60
\tmov rdi, 0
\tsyscall

print_digit:
\tmov rax, rbx
\tadd rax, 48
\tpush rax
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8
\tret

print_newline:
\tpush 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8
\tret
`,

  tests: [
    {
      name: "prints 3 and 7 with newlines",
      expected: "3\n7\n",
    },
  ],
};
