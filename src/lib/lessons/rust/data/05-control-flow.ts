import type { Lesson } from "../../types";

export const controlFlow: Lesson = {
  id: "control-flow",
  title: "Control Flow",
  chapterId: "basics",
  content: `## Control Flow in Rust

### if/else as an Expression

Unlike most languages, \`if\`/\`else\` in Rust is an **expression** — it returns a value:

\`\`\`rust
let number = 7;
let description = if number > 5 { "big" } else { "small" };
println!("{}", description); // big
\`\`\`

### loop

\`loop\` runs forever until you \`break\`:

\`\`\`rust
let mut counter = 0;
let result = loop {
    counter += 1;
    if counter == 10 {
        break counter * 2; // break can return a value
    }
};
\`\`\`

### while

\`\`\`rust
let mut n = 1;
while n < 100 {
    n *= 2;
}
\`\`\`

### for with ranges

\`\`\`rust
for i in 0..5 {       // 0, 1, 2, 3, 4 (exclusive end)
    print!("{} ", i);
}
for i in 0..=5 {      // 0, 1, 2, 3, 4, 5 (inclusive end)
    print!("{} ", i);
}
\`\`\`

### match

\`match\` is Rust's powerful pattern matching construct. Like \`if\`, it's an expression:

\`\`\`rust
let x = 3;
let msg = match x {
    1 => "one",
    2 | 3 => "two or three",
    4..=10 => "four to ten",
    _ => "something else",
};
\`\`\`

### Your Task

Implement three functions:

1. \`fizzbuzz(n: u32) -> String\` — returns "FizzBuzz" if divisible by both 3 and 5, "Fizz" if by 3, "Buzz" if by 5, otherwise the number as a string. Use \`match\`.
2. \`sum_range(start: u32, end: u32) -> u32\` — returns the sum of all integers from \`start\` to \`end\` inclusive.
3. \`collatz_steps(n: u64) -> u32\` — counts how many steps to reach 1 in the Collatz sequence: if n is even, divide by 2; if odd, multiply by 3 and add 1.`,

  starterCode: `fn fizzbuzz(n: u32) -> String {
    todo!()
}

fn sum_range(start: u32, end: u32) -> u32 {
    todo!()
}

fn collatz_steps(n: u64) -> u32 {
    todo!()
}

fn main() {
    println!("{}", fizzbuzz(15));
    println!("{}", sum_range(1, 10));
    println!("{}", collatz_steps(6));
}
`,

  solution: `fn fizzbuzz(n: u32) -> String {
    match (n % 3, n % 5) {
        (0, 0) => "FizzBuzz".to_string(),
        (0, _) => "Fizz".to_string(),
        (_, 0) => "Buzz".to_string(),
        _ => n.to_string(),
    }
}

fn sum_range(start: u32, end: u32) -> u32 {
    (start..=end).sum()
}

fn collatz_steps(mut n: u64) -> u32 {
    let mut steps = 0;
    while n != 1 {
        n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
        steps += 1;
    }
    steps
}

fn main() {
    println!("{}", fizzbuzz(15));
    println!("{}", sum_range(1, 10));
    println!("{}", collatz_steps(6));
}
`,

  tests: [
    {
      name: "fizzbuzz(15) returns FizzBuzz",
      expected: "FizzBuzz\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", fizzbuzz(15));
}`,
    },
    {
      name: "fizzbuzz(9) returns Fizz",
      expected: "Fizz\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", fizzbuzz(9));
}`,
    },
    {
      name: "fizzbuzz(10) returns Buzz",
      expected: "Buzz\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", fizzbuzz(10));
}`,
    },
    {
      name: "fizzbuzz(7) returns 7",
      expected: "7\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", fizzbuzz(7));
}`,
    },
    {
      name: "sum_range(1, 10) returns 55",
      expected: "55\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", sum_range(1, 10));
}`,
    },
    {
      name: "collatz_steps(6) returns 8",
      expected: "8\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", collatz_steps(6));
}`,
    },
  ],
};
