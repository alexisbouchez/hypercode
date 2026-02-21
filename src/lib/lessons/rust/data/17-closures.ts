import type { Lesson } from "../../types";

export const closures: Lesson = {
  id: "closures",
  title: "Closures",
  chapterId: "traits-generics",
  content: `## Closures

Closures are anonymous functions that can capture values from their surrounding scope.

### Syntax

\`\`\`rust
let add = |x, y| x + y;          // type inferred
let square = |x: i32| x * x;     // explicit type
let always_one = || 1;            // no parameters
\`\`\`

### Capturing the Environment

\`\`\`rust
let threshold = 5;
let greater_than_threshold = |n: &i32| n > &threshold; // captures threshold
\`\`\`

Closures can capture by reference, by mutable reference, or by value (\`move\`):

\`\`\`rust
let name = String::from("Alice");
let greet = move || println!("Hello, {}!", name); // name is moved into closure
greet();
\`\`\`

### Closure Traits

| Trait | What it means |
|-------|---------------|
| \`Fn\` | borrows immutably — can be called multiple times |
| \`FnMut\` | borrows mutably — can be called multiple times |
| \`FnOnce\` | consumes captures — can only be called once |

### Higher-Order Functions

Functions that accept closures use trait bounds:

\`\`\`rust
fn apply<F: Fn(i32) -> i32>(f: F, x: i32) -> i32 {
    f(x)
}
apply(|x| x * 2, 5); // 10
\`\`\`

### Returning Closures

\`\`\`rust
fn make_adder(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x + n
}
let add5 = make_adder(5);
add5(3); // 8
\`\`\`

### Your Task

1. \`apply<F: Fn(i32) -> i32>(f: F, x: i32) -> i32\` — applies f to x.
2. \`apply_n_times<F: Fn(i32) -> i32>(f: F, x: i32, n: u32) -> i32\` — applies f to x n times.
3. \`make_adder(n: i32) -> impl Fn(i32) -> i32\` — returns a closure that adds n.
4. \`make_multiplier(n: i32) -> impl Fn(i32) -> i32\` — returns a closure that multiplies by n.`,

  starterCode: `fn apply<F: Fn(i32) -> i32>(f: F, x: i32) -> i32 {
    todo!()
}

fn apply_n_times<F: Fn(i32) -> i32>(f: F, x: i32, n: u32) -> i32 {
    todo!()
}

fn make_adder(n: i32) -> impl Fn(i32) -> i32 {
    todo!()
}

fn make_multiplier(n: i32) -> impl Fn(i32) -> i32 {
    todo!()
}

fn main() {
    println!("{}", apply(|x| x * x, 5));
    println!("{}", apply_n_times(|x| x + 3, 10, 3));
    let add7 = make_adder(7);
    println!("{}", add7(10));
    let triple = make_multiplier(3);
    println!("{}", triple(6));
}
`,

  solution: `fn apply<F: Fn(i32) -> i32>(f: F, x: i32) -> i32 {
    f(x)
}

fn apply_n_times<F: Fn(i32) -> i32>(f: F, x: i32, n: u32) -> i32 {
    let mut result = x;
    for _ in 0..n {
        result = f(result);
    }
    result
}

fn make_adder(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x + n
}

fn make_multiplier(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x * n
}

fn main() {
    println!("{}", apply(|x| x * x, 5));
    println!("{}", apply_n_times(|x| x + 3, 10, 3));
    let add7 = make_adder(7);
    println!("{}", add7(10));
    let triple = make_multiplier(3);
    println!("{}", triple(6));
}
`,

  tests: [
    {
      name: "apply(|x| x*x, 5) returns 25",
      expected: "25\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", apply(|x| x * x, 5));
}`,
    },
    {
      name: "apply_n_times(|x| x+3, 10, 3) returns 19",
      expected: "19\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", apply_n_times(|x| x + 3, 10, 3));
}`,
    },
    {
      name: "make_adder(7)(10) returns 17",
      expected: "17\n",
      code: `{{FUNC}}
fn main() {
    let add7 = make_adder(7);
    println!("{}", add7(10));
}`,
    },
    {
      name: "make_multiplier(3)(6) returns 18",
      expected: "18\n",
      code: `{{FUNC}}
fn main() {
    let triple = make_multiplier(3);
    println!("{}", triple(6));
}`,
    },
    {
      name: "apply_n_times doubles 1 four times gives 16",
      expected: "16\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", apply_n_times(|x| x * 2, 1, 4));
}`,
    },
  ],
};
