import type { Lesson } from "../../types";

export const stackPushPop: Lesson = {
  id: "stack-push-pop",
  title: "The Stack: Push & Pop",
  chapterId: "functions",
  content: `## The Stack

The stack is a region of memory that grows **downward** (from high addresses to low). The \`rsp\` register always points to the top of the stack.

### Push and Pop

\`\`\`asm
push rax       ; rsp -= 8, then store rax at [rsp]
pop rbx        ; load [rsp] into rbx, then rsp += 8
\`\`\`

### LIFO Order

The stack is Last-In, First-Out. Values come back in reverse order:

\`\`\`asm
push 1         ; push first
push 2         ; push second
pop rcx        ; rcx = 2 (last pushed, first popped)
pop rdx        ; rdx = 1
\`\`\`

### Saving and Restoring Registers

A common use is saving registers you need to preserve:

\`\`\`asm
push rbx       ; save rbx
; ... use rbx freely ...
pop rbx        ; restore rbx
\`\`\`

### Using the Stack for Temporary Storage

You can write directly to the stack-allocated area:

\`\`\`asm
sub rsp, 8       ; allocate 8 bytes
mov BYTE [rsp], 72   ; store 'H' at [rsp]
; ... use the data ...
add rsp, 8       ; free the space
\`\`\`

### Your Task

Demonstrate that push/pop follows LIFO order:
1. Push 3, then push 1, then push 2 onto the stack
2. Pop all three values and print each as a digit followed by a newline
3. The output should show them in reverse push order: "2\\n1\\n3\\n"`,

  starterCode: `section .text
global _start
_start:
\t; Push three values
\tpush 3
\tpush 1
\tpush 2

\t; Pop first value into rbx, convert to ASCII, print
\tpop rbx
\tadd rbx, 48
\tpush rbx
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tpop rbx

\t; Print newline
\tpush 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8

\t; Pop second value and print digit + newline
\t; YOUR CODE HERE

\t; Pop third value and print digit + newline
\t; YOUR CODE HERE

\tmov rax, 60
\tmov rdi, 0
\tsyscall
`,

  solution: `section .text
global _start
_start:
\tpush 3
\tpush 1
\tpush 2

\tpop rbx
\tadd rbx, 48
\tpush rbx
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tpop rbx

\tpush 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8

\tpop rbx
\tadd rbx, 48
\tpush rbx
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tpop rbx

\tpush 10
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tadd rsp, 8

\tpop rbx
\tadd rbx, 48
\tpush rbx
\tmov rax, 1
\tmov rdi, 1
\tmov rsi, rsp
\tmov rdx, 1
\tsyscall
\tpop rbx

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
      name: "prints 2 1 3 (LIFO order)",
      expected: "2\n1\n3\n",
    },
  ],
};
