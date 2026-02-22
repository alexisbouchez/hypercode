import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "functions",
  content: `## Recursion

A function can call itself. The key is a **base case** that stops the recursion:

\`\`\`scala
def factorial(n: Int): Int =
  if (n <= 1) 1 else n * factorial(n - 1)

println(factorial(5))  // 120
\`\`\`

### Fibonacci

\`\`\`scala
def fib(n: Int): Int =
  if (n <= 1) n else fib(n - 1) + fib(n - 2)

println(fib(7))  // 13
\`\`\`

### Your Task

Write a recursive function \`power(base: Int, exp: Int): Int\` that returns \`base\` raised to \`exp\`. Assume \`exp >= 0\`. Base case: \`power(x, 0) = 1\`.`,

  starterCode: `def power(base: Int, exp: Int): Int =
  if (exp == 0) 1 else base * power(base, exp - 1)

println(power(2, 10))
println(power(3, 4))
`,

  solution: `def power(base: Int, exp: Int): Int =
  if (exp == 0) 1 else base * power(base, exp - 1)

println(power(2, 10))
println(power(3, 4))
`,

  tests: [
    {
      name: "2^10 = 1024",
      expected: "1024\n",
      code: `{{FUNC}}
println(power(2, 10))
`,
    },
    {
      name: "3^4 = 81",
      expected: "81\n",
      code: `{{FUNC}}
println(power(3, 4))
`,
    },
    {
      name: "5^0 = 1",
      expected: "1\n",
      code: `{{FUNC}}
println(power(5, 0))
`,
    },
  ],
};
