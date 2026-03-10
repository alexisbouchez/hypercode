import type { Lesson } from "../../types";

export const mulDiv: Lesson = {
  id: "mul-div",
  title: "Multiplication & Division",
  chapterId: "arithmetic",
  content: `## Multiplication

x86_64 has two multiply instructions:

### IMUL (Signed Multiply)

The most common form:
\`\`\`asm
imul rax, rbx        ; rax = rax * rbx
imul rax, rbx, 5     ; rax = rbx * 5
\`\`\`

### MUL (Unsigned Multiply)

One-operand form that uses \`rax\` implicitly:
\`\`\`asm
mul rbx              ; rdx:rax = rax * rbx
\`\`\`

The result is 128 bits, split across \`rdx\` (high) and \`rax\` (low).

## Division

### IDIV (Signed Divide) and DIV (Unsigned Divide)

Division also uses implicit registers:
\`\`\`asm
; Before dividing, sign-extend rax into rdx:rax
cqo                  ; sign-extend rax -> rdx:rax
idiv rbx             ; rax = rdx:rax / rbx, rdx = remainder
\`\`\`

For unsigned division:
\`\`\`asm
xor rdx, rdx        ; zero rdx (unsigned, no sign extension)
div rbx              ; rax = rdx:rax / rbx, rdx = remainder
\`\`\`

> **Important**: You must set up \`rdx\` before dividing! Use \`cqo\` for signed or \`xor rdx, rdx\` for unsigned.

### Your Task

Compute and print:
1. 3 * 3 = 9
2. 8 / 2 = 4
3. 7 % 3 = 1 (remainder)

Print: "9\\n4\\n1\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Compute 3 * 3 = 9
\tmov rax, 3
\timul rax, rax, 3
\t; Convert to ASCII and print
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\t; Compute 8 / 2 = 4
\t; YOUR CODE HERE

\t; Compute 7 % 3 = 1 (remainder is in rdx after div)
\t; YOUR CODE HERE

\tadd rsp, 8
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tmov rax, 3
\timul rax, rax, 3
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\tmov rax, 8
\txor rdx, rdx
\tmov rbx, 2
\tdiv rbx
\tadd rax, 48
\tmov [rsp], rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\tmov rax, 7
\txor rdx, rdx
\tmov rbx, 3
\tdiv rbx
\tadd rdx, 48
\tmov [rsp], rdx
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
`,

  tests: [
    {
      name: "prints 9 4 1",
      expected: "9\n4\n1\n",
    },
  ],
};
