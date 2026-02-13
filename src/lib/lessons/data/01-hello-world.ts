import type { Lesson } from "../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## The Anatomy of a Go Program

Every Go source file starts with a package declaration. A package is how Go organizes code: it serves as a namespace, a unit of compilation, and a mechanism for controlling visibility.

The \`main\` package is special. It tells the Go compiler that this is an executable program, not a library. Without it, you have a package that other code can import, but nothing you can actually run.

\`\`\`go
package main
\`\`\`

### Imports

The \`import\` keyword brings other packages into scope. The \`fmt\` package (short for "format") handles formatted I/O: printing to the terminal, formatting strings, reading input.

\`\`\`go
import "fmt"
\`\`\`

When you need multiple packages, Go uses a grouped syntax:

\`\`\`go
import (
    "fmt"
    "math"
    "strings"
)
\`\`\`

### The Entry Point

Every executable needs a starting point. In Go, that is \`func main()\`, with no parameters and no return value. When you run a Go program, execution begins here and here only.

> As Captain Picard would say: "Make it so." And with \`func main()\`, Go does exactly that.

\`\`\`go
func main() {
    fmt.Println("Hello, World!")
}
\`\`\`

\`fmt.Println\` writes its arguments to standard output, followed by a newline character.

### Exported Names

Notice the capital \`P\` in \`Println\`. In Go, any name that starts with an uppercase letter is *exported*, visible outside its package. A lowercase name is *unexported*, private to the package.

No \`public\` or \`private\` keywords. The casing **is** the access control. This is a deliberate design choice that makes visibility immediately obvious when reading code.

### Your Task

Write a program that prints exactly \`Hello, World!\` to standard output.`,

  starterCode: `package main

import "fmt"

func main() {
\t// Write your first Go program here
}
`,

  solution: `package main

import "fmt"

func main() {
\tfmt.Println("Hello, World!")
}
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
