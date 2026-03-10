import type { Lesson } from "../../types";

export const registers: Lesson = {
  id: "registers",
  title: "Registers",
  chapterId: "basics",
  content: `## x86_64 Registers

x86_64 processors have 16 general-purpose 64-bit registers. Each register can also be accessed as smaller sub-registers:

| 64-bit | 32-bit | 16-bit | 8-bit (low) |
|--------|--------|--------|-------------|
| \`rax\` | \`eax\` | \`ax\` | \`al\` |
| \`rbx\` | \`ebx\` | \`bx\` | \`bl\` |
| \`rcx\` | \`ecx\` | \`cx\` | \`cl\` |
| \`rdx\` | \`edx\` | \`dx\` | \`dl\` |
| \`rsi\` | \`esi\` | \`si\` | \`sil\` |
| \`rdi\` | \`edi\` | \`di\` | \`dil\` |
| \`rsp\` | \`esp\` | \`sp\` | \`spl\` |
| \`rbp\` | \`ebp\` | \`bp\` | \`bpl\` |
| \`r8\`  | \`r8d\` | \`r8w\` | \`r8b\` |
| ... | ... | ... | ... |
| \`r15\` | \`r15d\`| \`r15w\`| \`r15b\`|

### Important Rule

When you write to a 32-bit register (like \`eax\`), the upper 32 bits of the corresponding 64-bit register (\`rax\`) are **automatically zeroed**. This does not happen with 8-bit or 16-bit writes.

### Using \`mov\` to Set Registers

The \`mov\` instruction copies data:

\`\`\`asm
mov rax, 42       ; rax = 42
mov rbx, rax      ; rbx = rax (copy)
mov rcx, 0xFF     ; rcx = 255 (hex)
\`\`\`

### Your Task

Write a program that:
1. Stores the value 72 in \`rax\` (ASCII for 'H')
2. Stores 105 in \`rbx\` (ASCII for 'i')
3. Stores 10 in \`rcx\` (newline)
4. Writes the character 'H' to stdout, then 'i', then a newline

Hint: To write a single byte, you can store it in memory on the stack and point \`rsi\` at it.`,

  starterCode: `section .text
global _start
_start:
\t; Store 'H' (72) on the stack and print it
\tmov rax, 72
\tpush rax
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall

\t; Store 'i' (105) on the stack and print it
\t; YOUR CODE HERE

\t; Store newline (10) on the stack and print it
\t; YOUR CODE HERE

\t; Clean up stack and exit
\tadd rsp, 8
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tmov rax, 72
\tpush rax
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall

\tmov rax, 105
\tmov [rsp], rax
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall

\tmov rax, 10
\tmov [rsp], rax
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall

\tadd rsp, 8
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints Hi with newline",
      expected: "Hi\n",
    },
  ],
};
