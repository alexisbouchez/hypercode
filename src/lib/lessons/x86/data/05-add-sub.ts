import type { Lesson } from "../../types";

export const addSub: Lesson = {
  id: "add-sub",
  title: "Addition & Subtraction",
  chapterId: "arithmetic",
  content: `## Addition and Subtraction

The two most basic arithmetic instructions:

\`\`\`asm
add dst, src     ; dst = dst + src
sub dst, src     ; dst = dst - src
\`\`\`

Both update the flags register (ZF, SF, CF, OF) based on the result.

### Increment and Decrement

\`\`\`asm
inc rax          ; rax = rax + 1
dec rax          ; rax = rax - 1
\`\`\`

### NEG: Two's Complement Negation

\`\`\`asm
neg rax          ; rax = -rax
\`\`\`

### Computing and Printing a Number

To print a number, you need to convert it to ASCII digits. The simplest approach for single digits is to add 48 (ASCII '0'):

\`\`\`asm
mov rax, 7       ; the number 7
add rax, 48      ; convert to ASCII '7' (55)
\`\`\`

For this exercise, we will work with single-digit results.

### Your Task

Compute the following and print each result as a digit followed by a newline:
1. 3 + 4 = 7
2. 9 - 5 = 4
3. Start with 0, increment 6 times = 6

Print: "7\\n4\\n6\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Compute 3 + 4, convert to ASCII, store on stack, print
\tmov rax, 3
\tadd rax, 4
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\t; Compute 9 - 5, convert to ASCII, print
\t; YOUR CODE HERE

\t; Start at 0, increment 6 times, convert to ASCII, print
\t; YOUR CODE HERE

\t; Clean up and exit
\tadd rsp, 8
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tmov rax, 3
\tadd rax, 4
\tadd rax, 48
\tpush rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\tmov rax, 9
\tsub rax, 5
\tadd rax, 48
\tmov [rsp], rax
\tmov BYTE [rsp+1], 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\txor rax, rax
\tinc rax
\tinc rax
\tinc rax
\tinc rax
\tinc rax
\tinc rax
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
      name: "prints 7 4 6",
      expected: "7\n4\n6\n",
    },
  ],
};
