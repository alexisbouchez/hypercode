import type { Lesson } from "../../types";

export const option: Lesson = {
  id: "option",
  title: "The Option Type",
  chapterId: "structs-enums",
  content: `## Option<T>

Rust has no \`null\`. Instead, the absence of a value is represented with \`Option<T>\`:

\`\`\`rust
enum Option<T> {
    Some(T),
    None,
}
\`\`\`

This is part of the standard library and is always in scope.

### Creating Options

\`\`\`rust
let some_number: Option<i32> = Some(5);
let absent: Option<i32> = None;
\`\`\`

### Using Options

\`\`\`rust
// match — exhaustive handling
match some_number {
    Some(n) => println!("Got {}", n),
    None => println!("Nothing"),
}

// unwrap — panics if None
let n = some_number.unwrap();

// unwrap_or — provide a default
let n = some_number.unwrap_or(0);

// map — transform the value if Some
let doubled = some_number.map(|n| n * 2); // Some(10)

// if let — concise pattern match for one variant
if let Some(n) = some_number {
    println!("Got {}", n);
}
\`\`\`

### The ? Operator

In functions that return \`Option\`, use \`?\` to propagate \`None\` automatically:

\`\`\`rust
fn first_even_doubled(v: &[i32]) -> Option<i32> {
    let first_even = v.iter().find(|&&x| x % 2 == 0)?; // returns None if not found
    Some(first_even * 2)
}
\`\`\`

### Your Task

Implement four functions:

1. \`safe_divide(a: f64, b: f64) -> Option<f64>\` — returns None if b is 0.
2. \`first_even(numbers: &[i32]) -> Option<i32>\` — returns the first even number.
3. \`safe_sqrt(x: f64) -> Option<f64>\` — returns None for negative inputs.
4. \`double_first(v: &[i32]) -> Option<i32>\` — returns double the first element, or None if empty.`,

  starterCode: `fn safe_divide(a: f64, b: f64) -> Option<f64> {
    todo!()
}

fn first_even(numbers: &[i32]) -> Option<i32> {
    todo!()
}

fn safe_sqrt(x: f64) -> Option<f64> {
    todo!()
}

fn double_first(v: &[i32]) -> Option<i32> {
    todo!()
}

fn main() {
    println!("{}", safe_divide(10.0, 2.0).unwrap());
    println!("{}", safe_divide(1.0, 0.0).is_none());
    println!("{:?}", first_even(&[1, 3, 4, 6]));
    println!("{}", safe_sqrt(4.0).unwrap());
    println!("{:?}", double_first(&[5, 3, 1]));
}
`,

  solution: `fn safe_divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 { None } else { Some(a / b) }
}

fn first_even(numbers: &[i32]) -> Option<i32> {
    numbers.iter().copied().find(|&n| n % 2 == 0)
}

fn safe_sqrt(x: f64) -> Option<f64> {
    if x >= 0.0 { Some(x.sqrt()) } else { None }
}

fn double_first(v: &[i32]) -> Option<i32> {
    v.first().map(|&x| x * 2)
}

fn main() {
    println!("{}", safe_divide(10.0, 2.0).unwrap());
    println!("{}", safe_divide(1.0, 0.0).is_none());
    println!("{:?}", first_even(&[1, 3, 4, 6]));
    println!("{}", safe_sqrt(4.0).unwrap());
    println!("{:?}", double_first(&[5, 3, 1]));
}
`,

  tests: [
    {
      name: "safe_divide(10.0, 2.0) returns Some(5)",
      expected: "5\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", safe_divide(10.0, 2.0).unwrap());
}`,
    },
    {
      name: "safe_divide(1.0, 0.0) is None",
      expected: "true\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", safe_divide(1.0, 0.0).is_none());
}`,
    },
    {
      name: "first_even(&[1,3,4,6]) returns Some(4)",
      expected: "Some(4)\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", first_even(&[1, 3, 4, 6]));
}`,
    },
    {
      name: "first_even(&[1,3,5]) returns None",
      expected: "None\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", first_even(&[1, 3, 5]));
}`,
    },
    {
      name: "safe_sqrt(4.0) returns 2",
      expected: "2\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", safe_sqrt(4.0).unwrap());
}`,
    },
    {
      name: "double_first(&[5,3,1]) returns Some(10)",
      expected: "Some(10)\n",
      code: `{{FUNC}}
fn main() {
    println!("{:?}", double_first(&[5, 3, 1]));
}`,
    },
  ],
};
