import type { Lesson } from "../../types";

export const stringOps: Lesson = {
  id: "string-ops",
  title: "String Operations",
  chapterId: "advanced",
  content: `## Working with Strings

In assembly, strings are just sequences of bytes in memory. You manipulate them by loading and storing individual bytes.

### Reading Bytes from Memory

Use the \`BYTE\` size specifier to access individual bytes:

\`\`\`asm
movzx rax, BYTE [rsi]     ; load one byte, zero-extend to 64 bits
mov BYTE [rdi], al         ; store low byte of rax
\`\`\`

\`movzx\` (move with zero-extension) loads a small value and fills the upper bits with zeros.

### Iterating Over a String

To process each character of a null-terminated string:

\`\`\`asm
\tlea rsi, [mystring]
loop:
\tmovzx rax, BYTE [rsi]
\tcmp rax, 0            ; check for null terminator
\tje done
\t; ... process character in rax ...
\tinc rsi               ; advance to next character
\tjmp loop
done:
\`\`\`

### Computing String Length

Count characters until you hit a null byte (0) or a known terminator.

### Your Task

Write a program that computes the length of the string "Hello" (which is 5 characters) and prints it as a digit followed by a newline.

The string is stored in the data section with a null terminator. Loop through it, counting characters until you reach the null byte.

Print: "5\\n"`,

  starterCode: `section .data
mystr db "Hello", 0

section .text
global _start
_start:
\t; Count the length of mystr
\tlea rsi, [mystr]
\txor rbx, rbx          ; rbx = length counter = 0

count_loop:
\t; Load the byte at [rsi], check if zero, if not increment counter
\t; YOUR CODE HERE

count_done:
\t; rbx now holds the length (5)
\t; Convert to ASCII and print
\tmov rax, rbx
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
`,

  solution: `section .data
mystr db "Hello", 0

section .text
global _start
_start:
\tlea rsi, [mystr]
\txor rbx, rbx

count_loop:
\tmovzx rax, BYTE [rsi]
\tcmp rax, 0
\tje count_done
\tinc rbx
\tinc rsi
\tjmp count_loop

count_done:
\tmov rax, rbx
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
`,

  tests: [
    {
      name: "prints string length 5",
      expected: "5\n",
    },
  ],
};
