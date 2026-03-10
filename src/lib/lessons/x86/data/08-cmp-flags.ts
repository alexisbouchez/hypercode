import type { Lesson } from "../../types";

export const cmpFlags: Lesson = {
  id: "cmp-flags",
  title: "CMP & Flags",
  chapterId: "control-flow",
  content: `## Comparison and the Flags Register

The \`cmp\` instruction compares two values by subtracting them, but discards the result -- it only sets the flags:

\`\`\`asm
cmp rax, rbx       ; computes rax - rbx, sets flags
\`\`\`

### The Flags Register

After \`cmp\` or arithmetic, these flags are set:

| Flag | Name | Meaning |
|------|------|---------|
| ZF | Zero Flag | Result was zero (operands are equal) |
| SF | Sign Flag | Result was negative (high bit set) |
| CF | Carry Flag | Unsigned overflow/borrow |
| OF | Overflow Flag | Signed overflow |

### TEST Instruction

\`test\` performs a bitwise AND and sets flags (discards result):

\`\`\`asm
test rax, rax      ; sets ZF=1 if rax is zero
\`\`\`

This is the idiomatic way to check if a register is zero.

### Conditional Jumps After CMP

After \`cmp a, b\`:

| Jump | Condition | Meaning (signed) |
|------|-----------|-------------------|
| \`je\`/\`jz\` | ZF=1 | a == b |
| \`jne\`/\`jnz\` | ZF=0 | a != b |
| \`jg\` | ZF=0, SF=OF | a > b |
| \`jge\` | SF=OF | a >= b |
| \`jl\` | SF!=OF | a < b |
| \`jle\` | ZF=1 or SF!=OF | a <= b |

### Your Task

Compare numbers and print the result. For each comparison, print "Y" if true, "N" if false:
1. Is 5 == 5? (Y)
2. Is 3 > 7? (N)
3. Is 2 < 9? (Y)

Print: "Y\\nN\\nY\\n"`,

  starterCode: `section .data
yes db "Y", 10
no db "N", 10

section .text
global _start
_start:
\t; Is 5 == 5?
\tmov rax, 5
\tcmp rax, 5
\tje print_yes1
\tjmp print_no1
print_yes1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [yes]
\tmov rdx, 2
\tsyscall
\tjmp check2
print_no1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [no]
\tmov rdx, 2
\tsyscall
check2:
\t; Is 3 > 7?
\t; YOUR CODE HERE
\tjmp check3
print_yes2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [yes]
\tmov rdx, 2
\tsyscall
\tjmp check3
print_no2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [no]
\tmov rdx, 2
\tsyscall
check3:
\t; Is 2 < 9?
\t; YOUR CODE HERE
\tjmp done
print_yes3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [yes]
\tmov rdx, 2
\tsyscall
\tjmp done
print_no3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [no]
\tmov rdx, 2
\tsyscall
done:
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .data
yes db "Y", 10
no db "N", 10

section .text
global _start
_start:
\tmov rax, 5
\tcmp rax, 5
\tje print_yes1
\tjmp print_no1
print_yes1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [yes]
\tmov rdx, 2
\tsyscall
\tjmp check2
print_no1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [no]
\tmov rdx, 2
\tsyscall
check2:
\tmov rax, 3
\tcmp rax, 7
\tjg print_yes2
\tjmp print_no2
print_yes2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [yes]
\tmov rdx, 2
\tsyscall
\tjmp check3
print_no2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [no]
\tmov rdx, 2
\tsyscall
check3:
\tmov rax, 2
\tcmp rax, 9
\tjl print_yes3
\tjmp print_no3
print_yes3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [yes]
\tmov rdx, 2
\tsyscall
\tjmp done
print_no3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [no]
\tmov rdx, 2
\tsyscall
done:
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints Y N Y",
      expected: "Y\nN\nY\n",
    },
  ],
};
