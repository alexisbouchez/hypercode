import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "data-types",
  content: `## String Basics

Strings in Gleam are UTF-8 encoded and immutable. They are created with double quotes:

\`\`\`gleam
let greeting = "Hello, World!"
\`\`\`

### Concatenation with <>

The \`<>\` operator joins strings together:

\`\`\`gleam
let first = "Hello"
let second = "World"
let result = first <> ", " <> second <> "!"
// "Hello, World!"
\`\`\`

This is different from most languages which use \`+\` for string concatenation. The \`<>\` operator only works on strings -- you cannot accidentally concatenate a number with a string.

### The String Module

The \`gleam/string\` module provides functions for working with strings:

\`\`\`gleam
import gleam/string

string.length("hello")           // 5
string.uppercase("hello")        // "HELLO"
string.lowercase("HELLO")        // "hello"
string.reverse("hello")          // "olleh"
string.contains("hello", "ell")  // True
string.replace("hello", "l", "r") // "herro"
string.trim("  hello  ")         // "hello"
\`\`\`

### String Splitting and Joining

\`\`\`gleam
string.split("a,b,c", ",")       // ["a", "b", "c"]
string.join(["a", "b", "c"], "-") // "a-b-c"
\`\`\`

### Inspecting Values

The \`string.inspect\` function converts any value to a debug string representation:

\`\`\`gleam
string.inspect(42)     // "42"
string.inspect(True)   // "True"
string.inspect([1, 2]) // "[1, 2]"
\`\`\`

### Your Task

Write a function called \`shout\` that takes a string and returns it in uppercase with an exclamation mark at the end. Use it in \`main\` to shout "hello" and "gleam" on separate lines.`,

  starterCode: `import gleam/io
import gleam/string

fn shout(text: String) -> String {
\t// Convert to uppercase and add "!"
\t""
}

pub fn main() {
\tio.println(shout("hello"))
\tio.println(shout("gleam"))
}
`,

  solution: `import gleam/io
import gleam/string

fn shout(text: String) -> String {
\tstring.uppercase(text) <> "!"
}

pub fn main() {
\tio.println(shout("hello"))
\tio.println(shout("gleam"))
}
`,

  tests: [
    {
      name: "shouts hello and gleam",
      expected: "HELLO!\nGLEAM!\n",
    },
  ],
};
