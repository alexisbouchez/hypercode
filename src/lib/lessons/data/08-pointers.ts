import type { Lesson } from "../types";

export const pointers: Lesson = {
  id: "pointers",
  title: "Pointers",
  chapterId: "data-structures",
  content: `## Understanding Pointers

A pointer holds the memory address of a value. Instead of passing data around by copying it, you can pass a pointer to the original data.

### Pointer Types and Operators

The type \`*T\` is a pointer to a value of type \`T\`. The \`&\` operator takes the address of a variable. The \`*\` operator dereferences a pointer, giving you the value it points to:

\`\`\`go
x := 42
p := &x         // p is *int, points to x
fmt.Println(*p)  // 42 (read through the pointer)
*p = 100         // modify x through the pointer
fmt.Println(x)   // 100
\`\`\`

### Zero Value

The zero value of a pointer is \`nil\`. A nil pointer does not point to anything. Dereferencing a nil pointer causes a runtime panic:

\`\`\`go
var p *int       // p is nil
fmt.Println(p)   // <nil>
\`\`\`

### Passing by Value vs Pointer

Go is pass-by-value. When you pass a variable to a function, the function gets a copy. To let a function modify the original, pass a pointer:

\`\`\`go
func increment(x *int) {
    *x++
}

n := 5
increment(&n)
fmt.Println(n) // 6
\`\`\`

Without the pointer, \`increment\` would modify a copy and \`n\` would stay 5.

### When to Use Pointers

Use pointers when you need to:
- **Modify the caller's data** --- the most common reason
- **Avoid copying large structs** --- passing a pointer is cheaper than copying a large value
- **Signal absence** --- a nil pointer can mean "no value"

The \`new\` function allocates memory and returns a pointer to the zero value:

\`\`\`go
p := new(int)    // *int pointing to 0
*p = 42
\`\`\`

### Your Task

Write two functions:
- \`swap(a, b *int)\` --- swaps the values that \`a\` and \`b\` point to
- \`double(x *int)\` --- doubles the value that \`x\` points to`,

  starterCode: `package main

import "fmt"

func swap(a, b *int) {
\t// Your code here
}

func double(x *int) {
\t// Your code here
}

func main() {
\ta, b := 3, 5
\tswap(&a, &b)
\tfmt.Println(a, b)

\tx := 7
\tdouble(&x)
\tfmt.Println(x)
}
`,

  solution: `package main

import "fmt"

func swap(a, b *int) {
\t*a, *b = *b, *a
}

func double(x *int) {
\t*x *= 2
}

func main() {
\ta, b := 3, 5
\tswap(&a, &b)
\tfmt.Println(a, b)

\tx := 7
\tdouble(&x)
\tfmt.Println(x)
}
`,

  tests: [
    {
      name: "swap(3, 5) returns 5 3",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ta, b := 3, 5
\tswap(&a, &b)
\tfmt.Println(a, b)
}`,
      expected: "5 3\n",
    },
    {
      name: "swap(-1, 1) returns 1 -1",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\ta, b := -1, 1
\tswap(&a, &b)
\tfmt.Println(a, b)
}`,
      expected: "1 -1\n",
    },
    {
      name: "double(5) returns 10",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tx := 5
\tdouble(&x)
\tfmt.Println(x)
}`,
      expected: "10\n",
    },
    {
      name: "double(0) returns 0",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tx := 0
\tdouble(&x)
\tfmt.Println(x)
}`,
      expected: "0\n",
    },
  ],
};
