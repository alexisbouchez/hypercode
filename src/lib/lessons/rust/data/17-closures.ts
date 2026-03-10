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

### Capture Semantics

Rust closures capture variables from their environment in three ways, and the compiler picks the least restrictive mode that works:

**1. Borrow immutably (\`Fn\`)** — the closure only reads the captured value:

\`\`\`rust
let name = String::from("Alice");
let greet = || println!("Hello, {}", name); // borrows &name
greet();
greet(); // can call multiple times
println!("{}", name); // name still usable
\`\`\`

**2. Borrow mutably (\`FnMut\`)** — the closure modifies the captured value:

\`\`\`rust
let mut count = 0;
let mut inc = || { count += 1; count };
inc(); // count is now 1
inc(); // count is now 2
\`\`\`

**3. Move / consume (\`FnOnce\`)** — the closure takes ownership:

\`\`\`rust
let name = String::from("Alice");
let consume = || { drop(name); }; // moves name into closure
consume(); // name is dropped
// consume(); ← ERROR: cannot call FnOnce twice
\`\`\`

### The \`move\` Keyword

\`move\` forces the closure to take ownership of all captured variables, even if it only reads them. This is essential when the closure outlives the scope it was created in:

\`\`\`rust
fn make_greeter(name: String) -> impl Fn() -> String {
    move || format!("Hello, {}!", name) // must move: name would be dropped otherwise
}
\`\`\`

### Closure Trait Hierarchy

Every closure implements \`FnOnce\`. If it doesn't consume captures, it also implements \`FnMut\`. If it doesn't mutate captures, it also implements \`Fn\`:

\`\`\`
Fn ⊂ FnMut ⊂ FnOnce
\`\`\`

A function accepting \`impl FnOnce\` can take any closure. A function requiring \`impl Fn\` only accepts closures that borrow immutably.

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
4. \`make_multiplier(n: i32) -> impl Fn(i32) -> i32\` — returns a closure that multiplies by n.
5. \`make_counter() -> impl FnMut() -> i32\` — returns a closure that returns an incrementing count (1, 2, 3, ...) each time it is called. This exercises \`FnMut\` capture semantics.
6. \`consume_and_length(s: String) -> impl FnOnce() -> usize\` — returns a closure that moves \`s\` into itself and returns its length. This exercises \`FnOnce\` / \`move\` capture semantics.`,

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

fn make_counter() -> impl FnMut() -> i32 {
    todo!()
}

fn consume_and_length(s: String) -> impl FnOnce() -> usize {
    todo!()
}

fn main() {
    println!("{}", apply(|x| x * x, 5));
    println!("{}", apply_n_times(|x| x + 3, 10, 3));
    let add7 = make_adder(7);
    println!("{}", add7(10));
    let triple = make_multiplier(3);
    println!("{}", triple(6));
    let mut counter = make_counter();
    println!("{} {}", counter(), counter());
    let get_len = consume_and_length(String::from("Rust"));
    println!("{}", get_len());
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

fn make_counter() -> impl FnMut() -> i32 {
    let mut count = 0;
    move || { count += 1; count }
}

fn consume_and_length(s: String) -> impl FnOnce() -> usize {
    move || s.len()
}

fn main() {
    println!("{}", apply(|x| x * x, 5));
    println!("{}", apply_n_times(|x| x + 3, 10, 3));
    let add7 = make_adder(7);
    println!("{}", add7(10));
    let triple = make_multiplier(3);
    println!("{}", triple(6));
    let mut counter = make_counter();
    println!("{} {}", counter(), counter());
    let get_len = consume_and_length(String::from("Rust"));
    println!("{}", get_len());
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
    {
      name: "make_counter returns 1 then 2 then 3 (FnMut capture)",
      expected: "1 2 3\n",
      code: `{{FUNC}}
fn main() {
    let mut c = make_counter();
    println!("{} {} {}", c(), c(), c());
}`,
    },
    {
      name: "two independent counters track separately",
      expected: "1 1 2\n",
      code: `{{FUNC}}
fn main() {
    let mut c1 = make_counter();
    let mut c2 = make_counter();
    println!("{} {} {}", c1(), c2(), c1());
}`,
    },
    {
      name: "consume_and_length returns length of moved string (FnOnce)",
      expected: "4\n",
      code: `{{FUNC}}
fn main() {
    let f = consume_and_length(String::from("Rust"));
    println!("{}", f());
}`,
    },
    {
      name: "consume_and_length with empty string returns 0",
      expected: "0\n",
      code: `{{FUNC}}
fn main() {
    let f = consume_and_length(String::new());
    println!("{}", f());
}`,
    },
  ],
};
