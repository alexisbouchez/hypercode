import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "advanced-holyc",
  content: `## Recursion

Recursion is when a function calls itself to solve a problem by breaking it into smaller sub-problems. Every recursive function needs a **base case** — a condition that stops the recursion — and a **recursive case** that moves toward it.

### Factorial

The classic example: \`n! = n * (n-1) * ... * 1\`, with \`0! = 1\` as the base case.

\`\`\`holyc
I64 Factorial(I64 n) {
  if (n <= 1) return 1;
  return n * Factorial(n - 1);
}

Print("%d\\n", Factorial(5));  // 120
\`\`\`

When \`Factorial(5)\` is called, it expands as:
- \`5 * Factorial(4)\`
- \`5 * 4 * Factorial(3)\`
- \`5 * 4 * 3 * Factorial(2)\`
- \`5 * 4 * 3 * 2 * Factorial(1)\`
- \`5 * 4 * 3 * 2 * 1\` = \`120\`

### Fibonacci

The Fibonacci sequence is defined as: \`Fib(0) = 0\`, \`Fib(1) = 1\`, \`Fib(n) = Fib(n-1) + Fib(n-2)\`.

\`\`\`holyc
I64 Fib(I64 n) {
  if (n <= 0) return 0;
  if (n == 1) return 1;
  return Fib(n - 1) + Fib(n - 2);
}

Print("%d\\n", Fib(10));  // 55
\`\`\`

Note that this naive recursive Fibonacci has exponential time complexity — each call spawns two more calls. For large values of \`n\`, an iterative approach or memoization would be far more efficient.

### Base Cases Matter

Forgetting the base case creates infinite recursion, which will crash with a stack overflow. Always ask: "What is the simplest input, and what should it return?"

### Your Task

Write a function \`I64 SumTo(I64 n)\` that recursively computes the sum of all integers from \`1\` to \`n\`. The base case is: when \`n <= 0\`, return \`0\`.

Call it with \`10\` and print the result.

Expected output: \`55\``,

  starterCode: `// Define SumTo here

// Call it with 10 and print the result
`,

  solution: `I64 SumTo(I64 n) {
  if (n <= 0) return 0;
  return n + SumTo(n - 1);
}

Print("%d\\n", SumTo(10));
`,

  tests: [
    {
      name: "SumTo(10) returns 55",
      expected: "55\n",
    },
    {
      name: "SumTo(5) returns 15",
      code: '{{FUNC}}\nPrint("%d\\n", SumTo(5));',
      expected: "15\n",
    },
    {
      name: "SumTo(0) returns 0 (base case)",
      code: '{{FUNC}}\nPrint("%d\\n", SumTo(0));',
      expected: "0\n",
    },
  ],
};
