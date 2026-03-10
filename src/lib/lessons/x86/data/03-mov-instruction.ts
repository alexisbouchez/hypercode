import type { Lesson } from "../../types";

export const movInstruction: Lesson = {
  id: "mov-instruction",
  title: "The MOV Instruction",
  chapterId: "basics",
  content: `## The MOV Instruction

\`mov\` is the most fundamental x86_64 instruction. It copies data from a source to a destination.

### Forms of MOV

\`\`\`asm
mov reg, imm       ; Load immediate value into register
mov reg, reg       ; Copy register to register
mov reg, [mem]     ; Load from memory into register
mov [mem], reg     ; Store register into memory
mov [mem], imm     ; Store immediate into memory
\`\`\`

### Memory Addressing

x86_64 has powerful addressing modes:

\`\`\`asm
mov rax, [rbx]         ; Simple: address in rbx
mov rax, [rbx + 8]     ; Base + displacement
mov rax, [rbx + rcx]   ; Base + index
mov rax, [rbx + rcx*4] ; Base + scaled index
\`\`\`

### LEA vs MOV

\`lea\` (Load Effective Address) computes an address but does not access memory:

\`\`\`asm
lea rax, [msg]     ; rax = address of msg (no memory access)
mov rax, [msg]     ; rax = value AT address of msg (memory access)
\`\`\`

### The EQU Directive

You can define constants with \`equ\`:

\`\`\`asm
msg db "Hello", 10
msglen equ 6       ; constant: length of msg
\`\`\`

### Your Task

Write a program that defines two strings in the data section and prints them both. First print "x86_64 " (7 chars), then print "Assembly\\n" (9 chars).`,

  starterCode: `section .data
msg1 db "x86_64 "
msg1len equ 7
msg2 db "Assembly", 10
msg2len equ 9

section .text
global _start
_start:
\t; Print msg1
\t; YOUR CODE HERE

\t; Print msg2
\t; YOUR CODE HERE

\t; Exit
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .data
msg1 db "x86_64 "
msg1len equ 7
msg2 db "Assembly", 10
msg2len equ 9

section .text
global _start
_start:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [msg1]
\tmov rdx, msg1len
\tsyscall

\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [msg2]
\tmov rdx, msg2len
\tsyscall

\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints x86_64 Assembly",
      expected: "x86_64 Assembly\n",
    },
  ],
};
