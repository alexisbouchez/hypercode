import type { Lesson } from "../../types";

export const numbers: Lesson = {
  id: "numbers",
  title: "Numbers",
  chapterId: "data-types",
  content: `## Integers and Floats

Gleam has two numeric types: \`Int\` and \`Float\`. They are completely separate -- you cannot mix them in expressions.

### Integer Arithmetic

\`\`\`gleam
let a = 10
let b = 3
a + b   // 13
a - b   // 7
a * b   // 30
a / b   // 3  (integer division, truncates toward zero)
a % b   // 1  (remainder)
\`\`\`

You can use underscores for readability: \`1_000_000\`.

### Float Arithmetic

Float operators have a \`.\` suffix:

\`\`\`gleam
let x = 10.0
let y = 3.0
x +. y   // 13.0
x -. y   // 7.0
x *. y   // 30.0
x /. y   // 3.333...
\`\`\`

### The Int Module

The \`gleam/int\` module provides useful functions:

\`\`\`gleam
import gleam/int

int.to_string(42)          // "42"
int.to_float(42)           // 42.0
int.max(10, 20)            // 20
int.min(10, 20)            // 10
int.is_even(4)             // True
int.is_odd(3)              // True
int.absolute_value(-5)     // 5
\`\`\`

### The Float Module

\`\`\`gleam
import gleam/float

float.to_string(3.14)        // "3.14"
float.round(3.7)             // 4
float.floor(3.7)             // 3.0
float.ceiling(3.2)           // 4.0
float.truncate(3.7)          // 3
\`\`\`

### Comparison

Comparison operators work on both types, but you cannot compare an \`Int\` to a \`Float\`:

\`\`\`gleam
1 < 2     // True
1.0 <. 2.0 // True -- float comparisons use <. >. <=. >=.
1 == 1    // True
\`\`\`

### Your Task

Write a function called \`describe_number\` that takes an \`Int\` and returns:
- \`"positive"\` if the number is greater than 0
- \`"negative"\` if the number is less than 0
- \`"zero"\` if the number is 0

Call it in \`main\` with 42, -7, and 0, printing each result on a separate line.`,

  starterCode: `import gleam/io

fn describe_number(n: Int) -> String {
\t// Return "positive", "negative", or "zero"
\t""
}

pub fn main() {
\tio.println(describe_number(42))
\tio.println(describe_number(-7))
\tio.println(describe_number(0))
}
`,

  solution: `import gleam/io

fn describe_number(n: Int) -> String {
\tcase n > 0 {
\t\tTrue -> "positive"
\t\tFalse -> case n < 0 {
\t\t\tTrue -> "negative"
\t\t\tFalse -> "zero"
\t\t}
\t}
}

pub fn main() {
\tio.println(describe_number(42))
\tio.println(describe_number(-7))
\tio.println(describe_number(0))
}
`,

  tests: [
    {
      name: "describes positive, negative, and zero",
      expected: "positive\nnegative\nzero\n",
    },
  ],
};
