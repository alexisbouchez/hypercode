import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "control-flow",
  content: `## Recursion

Gleam has no \`for\` or \`while\` loops. Instead, you use **recursion** to repeat work. A recursive function calls itself with updated arguments until it reaches a **base case**.

### Basic Recursion

Here is a function that counts down from \`n\` to 1:

\`\`\`gleam
fn countdown(n: Int) -> Nil {
  case n <= 0 {
    True -> Nil
    False -> {
      io.println(int.to_string(n))
      countdown(n - 1)
    }
  }
}
\`\`\`

The base case is \`n <= 0\`, which stops the recursion. Without a base case, the function would recurse forever and crash.

### Accumulating Results

You can use an extra parameter (an **accumulator**) to build up a result:

\`\`\`gleam
fn sum(n: Int, acc: Int) -> Int {
  case n <= 0 {
    True -> acc
    False -> sum(n - 1, acc + n)
  }
}

// sum(5, 0) => 15  (5 + 4 + 3 + 2 + 1)
\`\`\`

This style is called **tail recursion** because the recursive call is the last thing the function does. Gleam optimizes tail-recursive functions so they use constant stack space, just like a loop.

### Looping with Recursion

A common pattern is a \`loop\` helper that iterates from one value to another:

\`\`\`gleam
fn loop(current: Int, max: Int) -> Nil {
  case current > max {
    True -> Nil
    False -> {
      io.println(int.to_string(current))
      loop(current + 1, max)
    }
  }
}
\`\`\`

Calling \`loop(1, 5)\` prints 1 through 5, one per line.

### Your Task

Write a function called \`factorial\` that takes an \`Int\` and returns its factorial as an \`Int\`. Recall that \`factorial(0) = 1\` and \`factorial(n) = n * factorial(n - 1)\`.

Print the factorial of 1, 5, and 10, each on a separate line.`,

  starterCode: `import gleam/io
import gleam/int

fn factorial(n: Int) -> Int {
\t// Implement factorial using recursion
\t0
}

pub fn main() {
\t// Print factorial of 1, 5, and 10
}
`,

  solution: `import gleam/io
import gleam/int

fn factorial(n: Int) -> Int {
\tcase n <= 0 {
\t\tTrue -> 1
\t\tFalse -> n * factorial(n - 1)
\t}
}

pub fn main() {
\tio.println(int.to_string(factorial(1)))
\tio.println(int.to_string(factorial(5)))
\tio.println(int.to_string(factorial(10)))
}
`,

  tests: [
    {
      name: "factorial of 1",
      expected: "1\n120\n3628800\n",
    },
    {
      name: "factorial of 5 via test code",
      code: `import gleam/io
import gleam/int

{{FUNC}}

pub fn main() {
\tio.println(int.to_string(factorial(5)))
}
`,
      expected: "120\n",
    },
    {
      name: "factorial of 0 (base case)",
      code: `import gleam/io
import gleam/int

{{FUNC}}

pub fn main() {
\tio.println(int.to_string(factorial(0)))
}
`,
      expected: "1\n",
    },
  ],
};
