import type { Lesson } from "../../types";

export const bitwiseOps: Lesson = {
  id: "bitwise-ops",
  title: "Bitwise Operations",
  chapterId: "arithmetic",
  content: `## Bitwise Operations

Bitwise instructions operate on individual bits:

| Instruction | Operation | Example |
|-------------|-----------|---------|
| \`and dst, src\` | Bitwise AND | \`and rax, 0xFF\` |
| \`or dst, src\` | Bitwise OR | \`or rax, rbx\` |
| \`xor dst, src\` | Bitwise XOR | \`xor rax, rax\` |
| \`not dst\` | Bitwise NOT | \`not rax\` |
| \`shl dst, count\` | Shift left | \`shl rax, 2\` |
| \`shr dst, count\` | Shift right (unsigned) | \`shr rax, 1\` |
| \`sar dst, count\` | Shift right (signed) | \`sar rax, 1\` |

### Common Patterns

**Zero a register** (fastest way):
\`\`\`asm
xor rax, rax      ; rax = 0 (faster than mov rax, 0)
\`\`\`

**Multiply by powers of 2**:
\`\`\`asm
shl rax, 3         ; rax = rax * 8
\`\`\`

**Divide by powers of 2**:
\`\`\`asm
shr rax, 2         ; rax = rax / 4 (unsigned)
\`\`\`

**Check if a bit is set** (using AND):
\`\`\`asm
and rax, 1         ; rax = lowest bit of rax (0 or 1)
\`\`\`

### Your Task

Compute and print:
1. 5 AND 3 = 1 (binary: 101 & 011 = 001)
2. 5 OR 2 = 7 (binary: 101 | 010 = 111)
3. 3 shifted left by 1 = 6 (binary: 011 << 1 = 110)

Print: "1\\n7\\n6\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Compute 5 AND 3
\tmov rax, 5
\tand rax, 3
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\t; Compute 5 OR 2
\t; YOUR CODE HERE

\t; Compute 3 SHL 1
\t; YOUR CODE HERE

\tadd rsp, 8
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tmov rax, 5
\tand rax, 3
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\tmov rax, 5
\tor rax, 2
\tadd rax, 48
\tmov [rsp], rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\tmov rax, 3
\tshl rax, 1
\tadd rax, 48
\tmov [rsp], rax
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
      name: "prints 1 7 6",
      expected: "1\n7\n6\n",
    },
  ],
};
