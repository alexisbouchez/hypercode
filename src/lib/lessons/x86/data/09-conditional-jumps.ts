import type { Lesson } from "../../types";

export const conditionalJumps: Lesson = {
  id: "conditional-jumps",
  title: "Conditional Jumps",
  chapterId: "control-flow",
  content: `## Conditional Jumps

Conditional jumps change the flow of execution based on flags. You typically set flags with \`cmp\` or \`test\`, then jump.

### Signed vs Unsigned Comparisons

For **signed** comparisons (treating values as positive or negative):
- \`jg\` / \`jge\` / \`jl\` / \`jle\`

For **unsigned** comparisons (treating values as always positive):
- \`ja\` / \`jae\` / \`jb\` / \`jbe\`

### If-Else Pattern

\`\`\`asm
\tcmp rax, 10
\tjl less_than       ; if rax < 10, jump
\t; else branch here
\tjmp end
less_than:
\t; if branch here
end:
\`\`\`

### Chained If-Else (if/elif/else)

\`\`\`asm
\tcmp rax, 0
\tjl negative
\tje zero
\t; positive branch
\tjmp end
negative:
\t; negative branch
\tjmp end
zero:
\t; zero branch
end:
\`\`\`

### Your Task

Write a program that classifies a number:
- If the number (stored in \`rax\`) is greater than 0, print "P\\n" (positive)
- If the number is 0, print "Z\\n" (zero)
- If the number is less than 0, print "N\\n" (negative)

Test with the value 42 -- it should print "P\\n".
Then test with 0 -- it should print "Z\\n".
Then test with -5 -- it should print "N\\n".

Print all three results: "P\\nZ\\nN\\n"`,

  starterCode: `section .data
pos db "P", 10
zero db "Z", 10
neg_s db "N", 10

section .text
global _start
_start:
\t; Test 1: classify 42
\tmov rax, 42
\t; YOUR CODE: compare and jump

\t; Test 2: classify 0
\t; YOUR CODE HERE

\t; Test 3: classify -5
\t; YOUR CODE HERE

\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .data
pos db "P", 10
zero db "Z", 10
neg_s db "N", 10

section .text
global _start
_start:
\tmov rax, 42
\tcmp rax, 0
\tjg is_pos1
\tje is_zero1
\tjmp is_neg1
is_pos1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [pos]
\tmov rdx, 2
\tsyscall
\tjmp test2
is_zero1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [zero]
\tmov rdx, 2
\tsyscall
\tjmp test2
is_neg1:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [neg_s]
\tmov rdx, 2
\tsyscall
test2:
\tmov rax, 0
\tcmp rax, 0
\tjg is_pos2
\tje is_zero2
\tjmp is_neg2
is_pos2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [pos]
\tmov rdx, 2
\tsyscall
\tjmp test3
is_zero2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [zero]
\tmov rdx, 2
\tsyscall
\tjmp test3
is_neg2:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [neg_s]
\tmov rdx, 2
\tsyscall
test3:
\tmov rax, -5
\tcmp rax, 0
\tjg is_pos3
\tje is_zero3
\tjmp is_neg3
is_pos3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [pos]
\tmov rdx, 2
\tsyscall
\tjmp done
is_zero3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [zero]
\tmov rdx, 2
\tsyscall
\tjmp done
is_neg3:
\tmov rax, 1
\tmov rdi, 1
\tlea rsi, [neg_s]
\tmov rdx, 2
\tsyscall
done:
\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  tests: [
    {
      name: "prints P Z N",
      expected: "P\nZ\nN\n",
    },
  ],
};
