import type { Lesson } from "../../types";

export const gcdAlgorithm: Lesson = {
  id: "gcd",
  title: "GCD Algorithm",
  chapterId: "challenges",
  content: `## Greatest Common Divisor

The **GCD** (Greatest Common Divisor) of two numbers is the largest number that divides both evenly. Euclid's algorithm computes it efficiently using repeated division.

### Euclid's Algorithm

The key insight: \`GCD(a, b) = GCD(b, a % b)\`. Keep replacing \`a\` with \`b\` and \`b\` with \`a % b\` until \`b\` becomes 0. Then \`a\` is the GCD.

\`\`\`
GCD(48, 18):
  48 % 18 = 12  →  GCD(18, 12)
  18 % 12 = 6   →  GCD(12, 6)
  12 % 6  = 0   →  GCD(6, 0)
  b = 0, so GCD = 6
\`\`\`

### Computing Remainder in ARM64

ARM64 has no modulo instruction, but you can compute \`a % b\` using:

\`\`\`asm
UDIV X2, X0, X1      // quotient = a / b
MUL X3, X2, X1       // quotient * b
SUB X0, X0, X3       // remainder = a - quotient * b
\`\`\`

### The GCD Loop

\`\`\`asm
// X0 = a, X1 = b
gcd_loop:
    CBZ X1, gcd_done      // if b == 0, GCD is in X0
    UDIV X2, X0, X1       // a / b
    MUL X3, X2, X1        // (a / b) * b
    SUB X2, X0, X3        // remainder = a % b
    MOV X0, X1            // a = old b
    MOV X1, X2            // b = remainder
    B gcd_loop
gcd_done:
// X0 = GCD
\`\`\`

### Your Task

Compute \`GCD(48, 18)\` using Euclid's algorithm and print the result (\`6\`) followed by a newline.`,

  starterCode: `.data
buf:
\t.skip 2

.text
.global _start
_start:
\tMOV X0, #48
\tMOV X1, #18

\t// Implement Euclid's algorithm:
\t// While b != 0: compute a % b, set a = b, b = remainder
\t// Print the GCD and exit
`,

  solution: `.data
buf:
\t.skip 2

.text
.global _start
_start:
\tMOV X0, #48
\tMOV X1, #18

gcd_loop:
\tCBZ X1, gcd_done
\tUDIV X2, X0, X1
\tMUL X3, X2, X1
\tSUB X2, X0, X3
\tMOV X0, X1
\tMOV X1, X2
\tB gcd_loop

gcd_done:
\tADD X0, X0, #48

\tLDR X9, =buf
\tSTRB W0, [X9]
\tMOV W1, #10
\tSTRB W1, [X9, #1]

\tMOV X0, #1
\tLDR X1, =buf
\tMOV X2, #2
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "GCD(48, 18) = 6",
      expected: "6\n",
    },
  ],
};
