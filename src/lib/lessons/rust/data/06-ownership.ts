import type { Lesson } from "../../types";

export const ownership: Lesson = {
  id: "ownership",
  title: "Ownership",
  chapterId: "ownership",
  content: `## Ownership

Ownership is Rust's most distinctive feature. It enables memory safety without a garbage collector.

### The Three Rules

1. Each value in Rust has an **owner**.
2. There can only be **one owner** at a time.
3. When the owner goes out of scope, the value is **dropped** (memory freed).

### Move Semantics

When you assign a heap-allocated value to another variable, ownership **moves**:

\`\`\`rust
let s1 = String::from("hello");
let s2 = s1; // s1 is MOVED into s2
// println!("{}", s1); // ERROR: s1 was moved
println!("{}", s2); // OK
\`\`\`

### Copy Types

Types that live entirely on the stack implement the \`Copy\` trait and are copied instead of moved:

\`\`\`rust
let x = 5;
let y = x; // x is COPIED, not moved
println!("{}", x); // OK — x still valid
\`\`\`

Integers, floats, booleans, and chars are \`Copy\`. \`String\` is not.

### Clone

To explicitly copy heap data, use \`.clone()\`:

\`\`\`rust
let s1 = String::from("hello");
let s2 = s1.clone(); // deep copy
println!("{} {}", s1, s2); // both valid
\`\`\`

### Ownership and Functions

Passing a value to a function moves or copies it, just like assignment:

\`\`\`rust
fn take_ownership(s: String) {
    println!("{}", s);
} // s is dropped here

fn makes_copy(x: i32) {
    println!("{}", x);
} // x is copied, original still valid
\`\`\`

### Your Task

Implement three functions:

1. \`make_greeting(name: &str) -> String\` — returns a greeting like "Hello, Alice!" using \`format!\`.
2. \`string_length(s: String) -> (String, usize)\` — takes ownership of a string, returns it together with its length.
3. \`repeat_string(s: &str, n: usize) -> String\` — returns the string repeated n times.`,

  starterCode: `fn make_greeting(name: &str) -> String {
    todo!()
}

fn string_length(s: String) -> (String, usize) {
    todo!()
}

fn repeat_string(s: &str, n: usize) -> String {
    todo!()
}

fn main() {
    println!("{}", make_greeting("Rust"));
    let (s, len) = string_length(String::from("world"));
    println!("{} {}", s, len);
    println!("{}", repeat_string("ab", 3));
}
`,

  solution: `fn make_greeting(name: &str) -> String {
    format!("Hello, {}!", name)
}

fn string_length(s: String) -> (String, usize) {
    let len = s.len();
    (s, len)
}

fn repeat_string(s: &str, n: usize) -> String {
    s.repeat(n)
}

fn main() {
    println!("{}", make_greeting("Rust"));
    let (s, len) = string_length(String::from("world"));
    println!("{} {}", s, len);
    println!("{}", repeat_string("ab", 3));
}
`,

  tests: [
    {
      name: "make_greeting(\"Rust\") returns Hello, Rust!",
      expected: "Hello, Rust!\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", make_greeting("Rust"));
}`,
    },
    {
      name: "make_greeting(\"World\") returns Hello, World!",
      expected: "Hello, World!\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", make_greeting("World"));
}`,
    },
    {
      name: "string_length returns string and its length",
      expected: "hello 5\n",
      code: `{{FUNC}}
fn main() {
    let (s, len) = string_length(String::from("hello"));
    println!("{} {}", s, len);
}`,
    },
    {
      name: "repeat_string(\"ab\", 3) returns ababab",
      expected: "ababab\n",
      code: `{{FUNC}}
fn main() {
    println!("{}", repeat_string("ab", 3));
}`,
    },
  ],
};
