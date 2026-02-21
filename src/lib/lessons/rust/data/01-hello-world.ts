import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "basics",
  content: `## Your First Rust Program

Every Rust program starts with a \`main\` function — the entry point where execution begins:

\`\`\`rust
fn main() {
    println!("Hello, World!");
}
\`\`\`

### The \`println!\` Macro

Notice the \`!\` after \`println\`. That exclamation mark means \`println!\` is a **macro**, not a regular function. Macros in Rust are special — they are expanded at compile time and can take a variable number of arguments.

\`println!\` prints a line of text followed by a newline character. It supports format strings:

\`\`\`rust
println!("Hello, {}!", "World");  // Hello, World!
println!("1 + 1 = {}", 2);        // 1 + 1 = 2
\`\`\`

### Comments

Single-line comments start with \`//\`:

\`\`\`rust
// This is a comment
fn main() {
    println!("Hello!"); // inline comment
}
\`\`\`

### Compilation Model

Rust is a compiled language. Before a Rust program can run, it must be compiled by \`rustc\` (the Rust compiler) into a native binary. Here in Hypercode, compilation happens automatically via Miri — a Rust interpreter that runs your code directly.

### Your Task

Print \`Hello, World!\` followed by \`Welcome to Rust!\` on separate lines.`,

  starterCode: `fn main() {
    // Print "Hello, World!" and "Welcome to Rust!"
}
`,

  solution: `fn main() {
    println!("Hello, World!");
    println!("Welcome to Rust!");
}
`,

  tests: [
    {
      name: "prints Hello, World! and Welcome to Rust!",
      expected: "Hello, World!\nWelcome to Rust!\n",
    },
  ],
};
