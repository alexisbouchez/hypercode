import type { Lesson } from "../../types";

export const dataTypes: Lesson = {
  id: "data-types",
  title: "Data Types",
  chapterId: "basics",
  content: `## Scalar Types

Rust has four primary scalar types:

### Integers

| Signed | Unsigned | Size |
|--------|----------|------|
| \`i8\` | \`u8\` | 8-bit |
| \`i32\` | \`u32\` | 32-bit (default) |
| \`i64\` | \`u64\` | 64-bit |
| \`isize\` | \`usize\` | pointer-sized |

### Floating Point

\`f64\` (64-bit, default) and \`f32\` (32-bit). Rust's math methods live on the types:

\`\`\`rust
let x: f64 = 2.0;
let y = x.sqrt();  // 1.4142...
let z = x.powi(3); // 8.0 (integer power)
\`\`\`

### Boolean and Char

\`\`\`rust
let t: bool = true;
let c: char = 'z'; // Unicode scalar value
\`\`\`

### Type Casting

Use \`as\` to cast between numeric types:

\`\`\`rust
let x: i32 = 42;
let y = x as f64; // 42.0
let z = 3.9f64 as i32; // 3 (truncates)
\`\`\`

## Compound Types

### Tuples

Fixed-length heterogeneous collections. Access with \`.0\`, \`.1\`, etc.:

\`\`\`rust
let t = (500, 6.4, true);
let (x, y, z) = t; // destructuring
println!("{}", t.0); // 500
\`\`\`

### Arrays

Fixed-length, same type. Stack-allocated:

\`\`\`rust
let a = [1, 2, 3, 4, 5];
let b: [i32; 3] = [0; 3]; // [0, 0, 0]
println!("{}", a[0]); // 1
\`\`\`

### Your Task

Implement these functions:

1. \`is_even(n: i32) -> bool\` — returns true if \`n\` is even.
2. \`absolute_value(x: i32) -> i32\` — returns the absolute value without using \`abs()\`.
3. \`hypotenuse(a: f64, b: f64) -> f64\` — returns the hypotenuse of a right triangle.
4. \`swap<T: Copy>(t: (T, T)) -> (T, T)\` — swaps the two elements of a tuple.`,

  starterCode: `fn is_even(n: i32) -> bool {
    todo!()
}

fn absolute_value(x: i32) -> i32 {
    todo!()
}

fn hypotenuse(a: f64, b: f64) -> f64 {
    todo!()
}

fn swap<T: Copy>(t: (T, T)) -> (T, T) {
    todo!()
}

fn main() {
    println!("{}", is_even(4));
    println!("{}", absolute_value(-7));
    println!("{:.4}", hypotenuse(3.0, 4.0));
    let t = swap((1i32, 2i32));
    println!("{} {}", t.0, t.1);
}
`,

  solution: `fn is_even(n: i32) -> bool {
    n % 2 == 0
}

fn absolute_value(x: i32) -> i32 {
    if x < 0 { -x } else { x }
}

fn hypotenuse(a: f64, b: f64) -> f64 {
    (a * a + b * b).sqrt()
}

fn swap<T: Copy>(t: (T, T)) -> (T, T) {
    (t.1, t.0)
}

fn main() {
    println!("{}", is_even(4));
    println!("{}", absolute_value(-7));
    println!("{:.4}", hypotenuse(3.0, 4.0));
    let t = swap((1i32, 2i32));
    println!("{} {}", t.0, t.1);
}
`,

  tests: [
    {
      name: "is_even(4) returns true",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", is_even(4));
}`,
    },
    {
      name: "is_even(7) returns false",
      expected: "false\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", is_even(7));
}`,
    },
    {
      name: "absolute_value(-7) returns 7",
      expected: "7\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", absolute_value(-7));
}`,
    },
    {
      name: "hypotenuse(3.0, 4.0) returns 5.0000",
      expected: "5.0000\n",
      code: `{{FUNC}}
fn main() {
    println!("{:.4}", hypotenuse(3.0, 4.0));
}`,
    },
    {
      name: "swap((1, 2)) returns (2, 1)",
      expected: "2 1\n",
      code: `{{FUNC}}
fn main() {
    let t = swap((1i32, 2i32));
    println!("{} {}", t.0, t.1);
}`,
    },
  ],
};
