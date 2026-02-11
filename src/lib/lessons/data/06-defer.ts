import type { Lesson } from "../types";

export const defer_: Lesson = {
  id: "defer",
  title: "Defer",
  chapterId: "functions",
  content: `## Cleaning Up After Yourself

The \`defer\` keyword schedules a function call to run when the surrounding function returns. It is Go's way of ensuring cleanup happens no matter how the function exits.

\`\`\`go
func main() {
    fmt.Println("start")
    defer fmt.Println("deferred")
    fmt.Println("end")
}
// Output:
// start
// end
// deferred
\`\`\`

The deferred call runs *after* the function body completes but *before* it returns to the caller.

### LIFO Order

When you defer multiple calls, they execute in **last-in, first-out** order --- like a stack:

\`\`\`go
func main() {
    defer fmt.Println("first")
    defer fmt.Println("second")
    defer fmt.Println("third")
}
// Output:
// third
// second
// first
\`\`\`

### Common Pattern: Open Then Defer Close

The most common use of \`defer\` is pairing resource acquisition with cleanup on the very next line:

\`\`\`go
f, err := os.Open("data.txt")
if err != nil {
    return err
}
defer f.Close()
// work with f...
\`\`\`

This makes it impossible to forget to close the file, regardless of how many return paths the function has.

### Arguments Are Evaluated Immediately

The arguments to a deferred call are evaluated when the \`defer\` statement executes, not when the deferred function runs:

\`\`\`go
x := 10
defer fmt.Println(x) // captures 10
x = 20
// prints 10, not 20
\`\`\`

### Defer in Loops

Be careful with \`defer\` inside loops. Deferred calls accumulate and only run when the function returns, not when the loop iteration ends:

\`\`\`go
for _, name := range files {
    f, _ := os.Open(name)
    defer f.Close() // these all pile up!
}
\`\`\`

If you need per-iteration cleanup, extract the body into a separate function so each deferred call runs promptly.

### Your Task

Write a function \`countdown(n int)\` that uses \`defer\` inside a loop to print numbers in reverse order, followed by \`"Go!"\`.

For example, \`countdown(3)\` should print:

\`\`\`
3
2
1
Go!
\`\`\`

Hint: defer each number inside the loop. Since deferred calls execute in LIFO order, deferring 1, 2, 3 will print 3, 2, 1. Print \`"Go!"\` at the end of the function body (not deferred) --- but think about why that does not work and what you need to do instead.`,

  starterCode: `package main

import "fmt"

func countdown(n int) {
\t// Your code here
}

func main() {
\tcountdown(3)
}
`,

  solution: `package main

import "fmt"

func countdown(n int) {
\tdefer fmt.Println("Go!")
\tfor i := 1; i <= n; i++ {
\t\tdefer fmt.Println(i)
\t}
}

func main() {
\tcountdown(3)
}
`,

  tests: [
    {
      name: "countdown(3) prints 3, 2, 1, Go!",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tcountdown(3)
}`,
      expected: "3\n2\n1\nGo!\n",
    },
    {
      name: "countdown(5) prints 5, 4, 3, 2, 1, Go!",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tcountdown(5)
}`,
      expected: "5\n4\n3\n2\n1\nGo!\n",
    },
    {
      name: "countdown(1) prints 1, Go!",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tcountdown(1)
}`,
      expected: "1\nGo!\n",
    },
  ],
};
