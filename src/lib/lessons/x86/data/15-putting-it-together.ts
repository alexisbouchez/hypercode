import type { Lesson } from "../../types";

export const puttingItTogether: Lesson = {
  id: "putting-it-together",
  title: "Putting It All Together",
  chapterId: "advanced",
  content: `## Putting It All Together

Time to combine everything you have learned: data sections, registers, arithmetic, control flow, functions, and string operations.

### Your Task

Write a program that computes the **factorial of 5** (5! = 120) and prints the result followed by a newline.

Since 120 is a multi-digit number, you will need to convert it to a string of ASCII digits. The algorithm:

1. Compute 5! = 120 using a loop or function
2. Convert 120 to the string "120" by repeatedly dividing by 10:
   - 120 / 10 = 12, remainder 0 -> digit '0'
   - 12 / 10 = 1, remainder 2 -> digit '2'
   - 1 / 10 = 0, remainder 1 -> digit '1'
3. The digits come out in reverse order, so store them on the stack and print in the right order

### Hints

- Use \`div\` with divisor 10 to extract digits
- Remember to \`xor rdx, rdx\` before each \`div\`
- Push each digit onto the stack as you extract it
- Count how many digits you pushed
- Print them all at once, or one at a time

Print: "120\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Compute 5! = 1 * 2 * 3 * 4 * 5
\tmov rax, 1       ; accumulator
\tmov rbx, 1       ; counter

fact_loop:
\tcmp rbx, 6
\tje fact_done
\timul rax, rbx
\tinc rbx
\tjmp fact_loop
fact_done:
\t; rax = 120

\t; Now convert rax to ASCII digits and print
\t; Use the stack to store digits in reverse
\tmov rbx, 10      ; divisor
\txor r12, r12     ; digit count

\t; YOUR CODE: extract digits by dividing by 10
\t; Push each digit (as ASCII) onto the stack
\t; Then print all digits followed by a newline

\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tmov rax, 1
\tmov rbx, 1

fact_loop:
\tcmp rbx, 6
\tje fact_done
\timul rax, rbx
\tinc rbx
\tjmp fact_loop
fact_done:

\tmov rbx, 10
\txor r12, r12

digit_loop:
\txor rdx, rdx
\tdiv rbx
\tadd rdx, 48
\tpush rdx
\tinc r12
\tcmp rax, 0
\tjne digit_loop

print_loop:
\tcmp r12, 0
\tje print_newline
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8
\tdec r12
\tjmp print_loop

print_newline:
\tpush 10
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
      name: "prints 120",
      expected: "120\n",
    },
  ],
};
