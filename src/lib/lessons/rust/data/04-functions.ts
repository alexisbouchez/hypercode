import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "basics",
  content: `## Functions in Rust

Functions are declared with \`fn\`, followed by the name, parameters, and an optional return type:

\`\`\`rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}
\`\`\`

### Expressions vs Statements

Rust is an **expression-based** language. The last expression in a function body is implicitly returned — no \`return\` keyword needed:

\`\`\`rust
fn double(x: i32) -> i32 {
    x * 2  // no semicolon — this is the return value
}
\`\`\`

Adding a semicolon turns an expression into a statement (which returns \`()\`):

\`\`\`rust
fn oops(x: i32) -> i32 {
    x * 2; // ERROR: expected i32, found ()
}
\`\`\`

### Early Return

Use \`return\` to return early:

\`\`\`rust
fn first_positive(v: &[i32]) -> Option<i32> {
    for &x in v {
        if x > 0 {
            return Some(x); // early return
        }
    }
    None // implicit return at the end
}
\`\`\`

### Recursion

Functions can call themselves:

\`\`\`rust
fn factorial(n: u64) -> u64 {
    if n <= 1 { 1 } else { n * factorial(n - 1) }
}
\`\`\`

### Your Task

Implement three functions:

1. \`max_of_three(a: i32, b: i32, c: i32) -> i32\` — returns the largest of three integers.
2. \`factorial(n: u64) -> u64\` — returns n! (factorial). Note: 0! = 1.
3. \`is_prime(n: u32) -> bool\` — returns true if n is a prime number.`,

  starterCode: `fn max_of_three(a: i32, b: i32, c: i32) -> i32 {
    todo!()
}

fn factorial(n: u64) -> u64 {
    todo!()
}

fn is_prime(n: u32) -> bool {
    todo!()
}

fn main() {
    println!("{}", max_of_three(3, 7, 5));
    println!("{}", factorial(6));
    println!("{}", is_prime(17));
    println!("{}", is_prime(4));
}
`,

  solution: `fn max_of_three(a: i32, b: i32, c: i32) -> i32 {
    if a >= b && a >= c { a }
    else if b >= c { b }
    else { c }
}

fn factorial(n: u64) -> u64 {
    if n <= 1 { 1 } else { n * factorial(n - 1) }
}

fn is_prime(n: u32) -> bool {
    if n < 2 { return false; }
    if n == 2 { return true; }
    if n % 2 == 0 { return false; }
    let mut i = 3u32;
    while i * i <= n {
        if n % i == 0 { return false; }
        i += 2;
    }
    true
}

fn main() {
    println!("{}", max_of_three(3, 7, 5));
    println!("{}", factorial(6));
    println!("{}", is_prime(17));
    println!("{}", is_prime(4));
}
`,

  tests: [
    {
      name: "max_of_three(3, 7, 5) returns 7",
      expected: "7\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", max_of_three(3, 7, 5));
}`,
    },
    {
      name: "max_of_three(10, 10, 5) returns 10",
      expected: "10\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", max_of_three(10, 10, 5));
}`,
    },
    {
      name: "factorial(6) returns 720",
      expected: "720\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", factorial(6));
}`,
    },
    {
      name: "factorial(0) returns 1",
      expected: "1\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", factorial(0));
}`,
    },
    {
      name: "is_prime(17) returns true",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", is_prime(17));
}`,
    },
    {
      name: "is_prime(4) returns false",
      expected: "false\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", is_prime(4));
}`,
    },
  ],
};
