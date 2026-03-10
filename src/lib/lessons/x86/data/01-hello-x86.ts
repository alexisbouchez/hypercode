import type { Lesson } from "../../types";

export const helloX86: Lesson = {
  id: "hello-x86",
  title: "Hello, x86_64!",
  chapterId: "basics",
  content: `## Your First x86_64 Program

x86_64 (also called AMD64) is the dominant desktop and server processor architecture. It powers nearly every laptop, desktop, and cloud server you use. Learning x86_64 assembly gives you a direct view into how your computer really works.

### Program Structure

An x86_64 assembly program in Intel syntax has two main sections:

- **\`section .data\`** -- holds your data (strings, numbers, buffers)
- **\`section .text\`** -- holds your code (instructions)

The \`_start\` label marks where execution begins. The \`global _start\` directive makes it visible to the system.

### Registers

x86_64 has 16 general-purpose 64-bit registers:

| Register | Typical Use |
|----------|-------------|
| \`rax\` | Return value, syscall number |
| \`rbx\` | Callee-saved general purpose |
| \`rcx\` | Counter for loops |
| \`rdx\` | Syscall arg 3, I/O |
| \`rsi\` | Syscall arg 2 (source index) |
| \`rdi\` | Syscall arg 1 (destination index) |
| \`rsp\` | Stack pointer |
| \`rbp\` | Base pointer (frame pointer) |
| \`r8\`-\`r15\` | Additional general purpose |

### Linux x86_64 Syscalls

On Linux x86_64, you invoke a syscall by:

1. Setting \`rax\` to the syscall number
2. Setting arguments in \`rdi\`, \`rsi\`, \`rdx\`, \`r10\`, \`r8\`, \`r9\`
3. Executing the \`syscall\` instruction

For \`sys_write\` (syscall 1):
- \`rdi\` = file descriptor (1 = stdout)
- \`rsi\` = pointer to the string in memory
- \`rdx\` = length of the string

For \`sys_exit\` (syscall 60):
- \`rdi\` = exit code

### Loading Addresses

To load the address of a data label into a register, use \`lea\` (load effective address):

\`\`\`asm
lea rsi, [msg]    ; rsi now holds the address of msg
\`\`\`

### Your Task

Write a program that prints exactly \`Hello, x86_64!\\n\` (15 characters) to stdout using the \`write\` syscall, then exits with code 0.`,

  starterCode: `section .data
msg db "Hello, x86_64!", 10

section .text
global _start
_start:
\t; Set up the write syscall
\t; rax = 1 (sys_write)
\t; rdi = 1 (stdout)
\t; rsi = address of msg
\t; rdx = 15 (length)
\t; syscall

\t; Then exit with code 0
\t; rax = 60 (sys_exit)
\t; rdi = 0 (exit code)
\t; syscall
`,

  solution: `section .data
msg db "Hello, x86_64!", 10

section .text
global _start
_start:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [msg]
\tmov rdx, 15
\tsyscall

\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints Hello, x86_64!",
      expected: "Hello, x86_64!\n",
    },
  ],
};
