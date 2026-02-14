import type { Lesson } from "../../types";

export const variablesAndTypes: Lesson = {
  id: "variables-and-types",
  title: "Variables and Types",
  chapterId: "foundations",
  content: `## Let Bindings

In Gleam, you create variables with \`let\`:

\`\`\`gleam
let name = "Gleam"
let age = 1
let pi = 3.14
let is_fun = True
\`\`\`

All variables in Gleam are **immutable**. Once you bind a value to a name, it cannot be changed. You can, however, rebind a name to a new value:

\`\`\`gleam
let x = 1
let x = x + 1  // This creates a new binding, x is now 2
\`\`\`

### Basic Types

Gleam has these fundamental types:

| Type | Examples | Description |
|------|----------|-------------|
| \`Int\` | \`42\`, \`-7\`, \`1_000_000\` | Arbitrary precision integers |
| \`Float\` | \`3.14\`, \`-0.5\`, \`1.0e10\` | 64-bit floating point |
| \`String\` | \`"hello"\` | UTF-8 strings |
| \`Bool\` | \`True\`, \`False\` | Boolean values |

### Type Annotations

Gleam can infer types, but you can add annotations for clarity:

\`\`\`gleam
let name: String = "Gleam"
let age: Int = 1
\`\`\`

### Int and Float Are Separate

Gleam strictly separates integers and floats. You cannot mix them in arithmetic:

\`\`\`gleam
let x = 1 + 2      // Int arithmetic: +, -, *, /, %
let y = 1.0 +. 2.0  // Float arithmetic: +., -., *., /.
\`\`\`

The \`.\` suffix on operators indicates float operations. This is different from most languages but prevents subtle precision bugs.

### Converting Between Types

Use \`int.to_string\` and \`float.to_string\` to convert numbers to strings:

\`\`\`gleam
import gleam/int
import gleam/float

let age_str = int.to_string(42)       // "42"
let pi_str = float.to_string(3.14)    // "3.14"
\`\`\`

### Your Task

Create variables for your name and your favorite number. Print them on separate lines in the format:
\`\`\`
Name: Lucy
Number: 7
\`\`\``,

  starterCode: `import gleam/io
import gleam/int

pub fn main() {
\tlet name = "Lucy"
\t// Create a variable for your favorite number
\t// Print "Name: " followed by the name
\t// Print "Number: " followed by the number
}
`,

  solution: `import gleam/io
import gleam/int

pub fn main() {
\tlet name = "Lucy"
\tlet number = 7
\tio.println("Name: " <> name)
\tio.println("Number: " <> int.to_string(number))
}
`,

  tests: [
    {
      name: "prints name and number",
      expected: "Name: Lucy\nNumber: 7\n",
    },
  ],
};
