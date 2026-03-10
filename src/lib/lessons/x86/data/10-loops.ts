import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## Loops in Assembly

There is no \`for\` or \`while\` keyword in assembly. Loops are built from comparisons and jumps.

### While Loop Pattern

\`\`\`asm
loop_start:
\tcmp rcx, 0
\tje loop_end
\t; ... loop body ...
\tdec rcx
\tjmp loop_start
loop_end:
\`\`\`

### Counting Up

\`\`\`asm
\txor rbx, rbx         ; counter = 0
loop_top:
\tcmp rbx, 5
\tje loop_done
\t; ... do work ...
\tinc rbx
\tjmp loop_top
loop_done:
\`\`\`

### Preserving Registers Across Syscalls

The \`syscall\` instruction clobbers \`rcx\` and \`r11\`. If you need a loop counter to survive across a syscall, use a callee-saved register like \`rbx\`, \`r12\`-\`r15\`, or save/restore with \`push\`/\`pop\`.

### Your Task

Write a program that uses a loop to print the digits 1 through 5, each followed by a newline.

Print: "1\\n2\\n3\\n4\\n5\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Allocate space on stack for our output buffer
\tsub rsp, 16

\t; Use rbx as loop counter (1 to 5)
\tmov rbx, 1

loop_top:
\tcmp rbx, 6
\tje loop_done

\t; Convert counter to ASCII digit, store on stack, print
\t; YOUR CODE HERE

\t; Increment and loop
\tinc rbx
\tjmp loop_top

loop_done:
\tadd rsp, 16
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tsub rsp, 16
\tmov rbx, 1

loop_top:
\tcmp rbx, 6
\tje loop_done

\tmov rax, rbx
\tadd rax, 48
\tmov [rsp], rax
\tmov BYTE [rsp+1], 10

\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 2
\tsyscall

\tinc rbx
\tjmp loop_top

loop_done:
\tadd rsp, 16
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints 1 through 5",
      expected: "1\n2\n3\n4\n5\n",
    },
  ],
};
