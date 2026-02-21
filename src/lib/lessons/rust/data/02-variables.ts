import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables and Mutability",
  chapterId: "basics",
  content: `## Variables in Rust

In Rust, variables are declared with \`let\` and are **immutable by default** — once bound to a value, they cannot be changed:

\`\`\`rust
let x = 5;
// x = 6; // ERROR: cannot assign twice to immutable variable
\`\`\`

To make a variable mutable, add \`mut\`:

\`\`\`rust
let mut count = 0;
count += 1; // OK
\`\`\`

### Type Inference

Rust infers types from context — you don't always need to annotate them:

\`\`\`rust
let x = 42;       // i32
let y = 3.14;     // f64
let name = "Rust"; // &str
\`\`\`

But you can be explicit:

\`\`\`rust
let x: i32 = 42;
let y: f64 = 3.14;
\`\`\`

### Shadowing

Rust lets you declare a new variable with the same name, **shadowing** the previous one. Shadowing is different from mutation — it creates a brand new variable:

\`\`\`rust
let x = 5;
let x = x + 1;   // shadows the previous x
let x = x * 2;   // shadows again
println!("{}", x); // 12
\`\`\`

Shadowing allows changing the type, which \`mut\` does not:

\`\`\`rust
let spaces = "   ";      // &str
let spaces = spaces.len(); // usize — OK with shadowing
\`\`\`

### Constants

Constants are always immutable and must have a type annotation. They can be defined in any scope:

\`\`\`rust
const MAX_POINTS: u32 = 100_000;
\`\`\`

### Your Task

Implement two functions:

1. \`shadowing(x: i32) -> i32\` — shadows the parameter: first add 1, then multiply by 2, return the result.
2. \`celsius_to_fahrenheit(c: f64) -> f64\` — converts Celsius to Fahrenheit using the formula \`c * 9.0 / 5.0 + 32.0\`.`,

  starterCode: `fn shadowing(x: i32) -> i32 {
    // Shadow x: first add 1, then multiply by 2
    todo!()
}

fn celsius_to_fahrenheit(c: f64) -> f64 {
    // c * 9.0 / 5.0 + 32.0
    todo!()
}

fn main() {
    println!("{}", shadowing(5));
    println!("{:.1}", celsius_to_fahrenheit(0.0));
}
`,

  solution: `fn shadowing(x: i32) -> i32 {
    let x = x + 1;
    let x = x * 2;
    x
}

fn celsius_to_fahrenheit(c: f64) -> f64 {
    c * 9.0 / 5.0 + 32.0
}

fn main() {
    println!("{}", shadowing(5));
    println!("{:.1}", celsius_to_fahrenheit(0.0));
}
`,

  tests: [
    {
      name: "shadowing(5) returns 12",
      expected: "12\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", shadowing(5));
}`,
    },
    {
      name: "shadowing(0) returns 2",
      expected: "2\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", shadowing(0));
}`,
    },
    {
      name: "celsius_to_fahrenheit(0.0) returns 32.0",
      expected: "32.0\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.1}", celsius_to_fahrenheit(0.0));
}`,
    },
    {
      name: "celsius_to_fahrenheit(100.0) returns 212.0",
      expected: "212.0\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.1}", celsius_to_fahrenheit(100.0));
}`,
    },
  ],
};
