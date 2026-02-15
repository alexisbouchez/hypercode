import type { Lesson } from "../../types";

export const higherOrderFunctions: Lesson = {
  id: "higher-order-functions",
  title: "Higher-Order Functions",
  chapterId: "functions",
  content: `## Functions as Values

In R, functions are first-class objects. You can pass them as arguments, return them from other functions, and store them in variables.

### Passing Functions

\`\`\`r
apply_twice <- function(f, x) {
  f(f(x))
}

double <- function(x) x * 2
cat(apply_twice(double, 3), "\\n")  # 12
\`\`\`

### Anonymous Functions

You can create functions inline without naming them:

\`\`\`r
result <- sapply(1:5, function(x) x ^ 2)
cat(result, "\\n")  # 1 4 9 16 25
\`\`\`

R 4.1+ also supports a shorthand syntax with \`\\()\`:

\`\`\`r
result <- sapply(1:5, \\(x) x ^ 2)
\`\`\`

### Closures

Functions in R capture their enclosing environment:

\`\`\`r
make_adder <- function(n) {
  function(x) x + n
}

add5 <- make_adder(5)
cat(add5(3), "\\n")   # 8
cat(add5(10), "\\n")  # 15
\`\`\`

The inner function remembers the value of \`n\` from when \`make_adder\` was called.

### Your Task

Write a function \`make_multiplier\` that takes a number \`n\` and returns a function that multiplies its argument by \`n\`. Use it to create \`triple\` (multiplies by 3) and print \`triple(4)\` and \`triple(7)\`.`,

  starterCode: `make_multiplier <- function(n) {
\t# Return a function that multiplies by n
}

triple <- make_multiplier(3)
cat(triple(4), "\\n")
cat(triple(7), "\\n")
`,

  solution: `make_multiplier <- function(n) {
\tfunction(x) x * n
}

triple <- make_multiplier(3)
cat(triple(4), "\\n")
cat(triple(7), "\\n")
`,

  tests: [
    {
      name: "multiplier closure works",
      expected: "12 \n21 \n",
    },
  ],
};
