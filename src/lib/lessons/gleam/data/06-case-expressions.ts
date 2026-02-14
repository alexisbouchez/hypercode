import type { Lesson } from "../../types";

export const caseExpressions: Lesson = {
  id: "case-expressions",
  title: "Case Expressions",
  chapterId: "control-flow",
  content: `## Case Expressions

Gleam does not have \`if/else\` statements in the traditional sense. Instead, it uses \`case\` expressions for all branching logic:

\`\`\`gleam
case value {
  pattern1 -> expression1
  pattern2 -> expression2
  _ -> default_expression
}
\`\`\`

### Matching on Values

You can match on specific values:

\`\`\`gleam
fn describe_day(day: Int) -> String {
  case day {
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"
    _ -> "Unknown"
  }
}
\`\`\`

The \`_\` wildcard matches anything and acts as the default case.

### Matching on Booleans

Since Gleam has no \`if/else\`, you use \`case\` with booleans:

\`\`\`gleam
let message = case temperature > 30 {
  True -> "It's hot!"
  False -> "It's fine."
}
\`\`\`

### Case Is an Expression

Every \`case\` returns a value, so you can bind the result to a variable:

\`\`\`gleam
let grade = case score {
  score if score >= 90 -> "A"
  score if score >= 80 -> "B"
  score if score >= 70 -> "C"
  _ -> "F"
}
\`\`\`

### Multiple Patterns

You can match multiple values at once using a tuple pattern:

\`\`\`gleam
case x, y {
  0, 0 -> "origin"
  0, _ -> "y-axis"
  _, 0 -> "x-axis"
  _, _ -> "elsewhere"
}
\`\`\`

### Guards

You can add conditions to patterns with \`if\`:

\`\`\`gleam
case number {
  n if n > 0 -> "positive"
  n if n < 0 -> "negative"
  _ -> "zero"
}
\`\`\`

### Your Task

Write a function called \`fizzbuzz\` that takes an \`Int\` and returns:
- \`"FizzBuzz"\` if divisible by both 3 and 5
- \`"Fizz"\` if divisible by 3
- \`"Buzz"\` if divisible by 5
- The number as a string otherwise

Print the result for numbers 1 through 15, each on a separate line.`,

  starterCode: `import gleam/io
import gleam/int

fn fizzbuzz(n: Int) -> String {
\t// Implement FizzBuzz
\t""
}

pub fn main() {
\t// Print fizzbuzz for 1 through 15
}
`,

  solution: `import gleam/io
import gleam/int

fn fizzbuzz(n: Int) -> String {
\tcase n % 3, n % 5 {
\t\t0, 0 -> "FizzBuzz"
\t\t0, _ -> "Fizz"
\t\t_, 0 -> "Buzz"
\t\t_, _ -> int.to_string(n)
\t}
}

fn loop(current: Int, max: Int) -> Nil {
\tcase current > max {
\t\tTrue -> Nil
\t\tFalse -> {
\t\t\tio.println(fizzbuzz(current))
\t\t\tloop(current + 1, max)
\t\t}
\t}
}

pub fn main() {
\tloop(1, 15)
}
`,

  tests: [
    {
      name: "prints FizzBuzz for 1-15",
      expected: "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n",
    },
  ],
};
