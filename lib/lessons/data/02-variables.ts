import type { Lesson } from "../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables",
  chapterId: "foundations",
  content: `## Declaring Variables

Go is statically typed, but it does not force you to spell out every type. You have two primary ways to declare variables.

### The \`var\` Keyword

The explicit form. You state the name, the type, and optionally an initial value:

\`\`\`go
var name string = "Go"
var year int = 2009
var ratio float64 = 3.14
\`\`\`

If you provide an initial value, the type can be omitted --- the compiler infers it:

\`\`\`go
var name = "Go"     // inferred as string
var year = 2009     // inferred as int
\`\`\`

### Short Declaration

Inside functions, the \`:=\` operator declares and initializes in one step. This is the form you will use most often:

\`\`\`go
name := "Go"
year := 2009
awesome := true
\`\`\`

The \`:=\` operator is only available inside functions. Package-level variables must use \`var\`.

### Zero Values

Every type in Go has a *zero value* --- the value a variable holds if you declare it without initializing it. This is a guarantee, not an accident. There are no uninitialized variables in Go.

| Type | Zero Value |
|------|-----------|
| \`int\`, \`float64\` | \`0\` |
| \`string\` | \`""\` (empty string) |
| \`bool\` | \`false\` |
| pointers, slices, maps | \`nil\` |

\`\`\`go
var count int     // 0
var label string  // ""
var ready bool    // false
\`\`\`

### Constants

Values that never change are declared with \`const\`. Constants must be known at compile time --- you cannot assign the result of a function call to a constant.

\`\`\`go
const pi = 3.14159
const maxRetries = 3
\`\`\`

### Your Task

Declare three variables using the short declaration operator:
- \`name\` with value \`"Go"\`
- \`year\` with value \`2009\`
- \`awesome\` with value \`true\`

Then print them using the format string provided in the starter code.`,

  starterCode: `package main

import "fmt"

func main() {
\t// Declare your variables here using :=

\tfmt.Printf("name: %s, year: %d, awesome: %t\\n", name, year, awesome)
}
`,

  solution: `package main

import "fmt"

func main() {
\tname := "Go"
\tyear := 2009
\tawesome := true

\tfmt.Printf("name: %s, year: %d, awesome: %t\\n", name, year, awesome)
}
`,

  tests: [
    {
      name: "prints formatted variables",
      expected: "name: Go, year: 2009, awesome: true\n",
    },
  ],
};
