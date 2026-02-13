import type { Lesson } from "../../types";

export const helloArm64: Lesson = {
  id: "hello-arm64",
  title: "Hello, ARM64!",
  chapterId: "foundations",
  content: `## Your First ARM64 Program

ARM64 (also called AArch64) is the 64-bit instruction set used by billions of devices worldwide -- from smartphones to Apple's M-series chips to cloud servers. Learning ARM64 assembly gives you a direct understanding of how processors actually execute code.

### Registers

ARM64 has 31 general-purpose 64-bit registers named \`X0\` through \`X30\`, plus a few special ones:

| Register | Purpose |
|----------|---------|
| \`X0\`-\`X7\` | Function arguments and return values |
| \`X8\` | Syscall number |
| \`X9\`-\`X15\` | Temporary (caller-saved) |
| \`X16\`-\`X18\` | Platform reserved |
| \`X19\`-\`X28\` | Callee-saved |
| \`X29\` (\`FP\`) | Frame pointer |
| \`X30\` (\`LR\`) | Link register (return address) |
| \`SP\` | Stack pointer |
| \`XZR\` | Zero register (always reads as 0) |

### Writing to the Screen

On Linux AArch64, you print text using the \`write\` syscall. To invoke a syscall, you:

1. Set \`X8\` to the syscall number (64 for \`write\`)
2. Set the arguments in \`X0\`, \`X1\`, \`X2\`
3. Execute \`SVC #0\` (supervisor call)

For \`write\`, the arguments are:
- \`X0\` = file descriptor (1 = stdout)
- \`X1\` = pointer to the string in memory
- \`X2\` = length of the string

### The .data Section

String data lives in the \`.data\` section. You define a label and use \`.asciz\` (null-terminated string) or \`.ascii\` (no null terminator):

\`\`\`asm
.data
msg:
    .asciz "Hello, ARM64!\\n"
\`\`\`

### Loading Addresses

To load the address of a label into a register, use \`LDR\` with the \`=\` prefix:

\`\`\`asm
LDR X1, =msg    // X1 now holds the address of msg
\`\`\`

### A Complete Program

\`\`\`asm
.data
msg:
    .ascii "Hello, ARM64!\\n"

.text
.global _start
_start:
    MOV X0, #1        // fd = stdout
    LDR X1, =msg      // buf = address of msg
    MOV X2, #14        // count = 14 bytes
    MOV X8, #64        // syscall = write
    SVC #0             // invoke syscall

    MOV X0, #0         // exit code = 0
    MOV X8, #93        // syscall = exit
    SVC #0
\`\`\`

### Your Task

Write a program that prints exactly \`Hello, ARM64!\\n\` (14 characters) to stdout using the \`write\` syscall, then exits with code 0.`,

  starterCode: `.data
msg:
\t.ascii "Hello, ARM64!\\n"

.text
.global _start
_start:
\t// Set up the write syscall
\t// X0 = 1 (stdout)
\t// X1 = address of msg
\t// X2 = 14 (length)
\t// X8 = 64 (write syscall number)
\t// SVC #0

\t// Then exit with code 0
\t// X0 = 0 (exit code)
\t// X8 = 93 (exit syscall number)
\t// SVC #0
`,

  solution: `.data
msg:
\t.ascii "Hello, ARM64!\\n"

.text
.global _start
_start:
\tMOV X0, #1
\tLDR X1, =msg
\tMOV X2, #14
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "prints Hello, ARM64!",
      expected: "Hello, ARM64!\n",
    },
  ],
};
