import type { Lesson } from "../types";

export const genericFunctions: Lesson = {
  id: "generic-functions",
  title: "Generic Functions",
  chapterId: "generics",
  content: `## Type Parameters

Go 1.18 introduced generics, allowing you to write functions and types that work with any type. Before generics, you had to write separate functions for each type or use \`interface{}\` and lose type safety.

### Syntax

A generic function declares one or more *type parameters* in square brackets before the regular parameters:

\`\`\`go
func Print[T any](val T) {
    fmt.Println(val)
}
\`\`\`

\`T\` is a type parameter. \`any\` is a *constraint* that means "any type at all". The function can be called with any type:

\`\`\`go
Print[int](42)
Print[string]("hello")
\`\`\`

### Type Inference

In most cases the compiler can figure out the type argument from the regular arguments, so you can omit it:

\`\`\`go
Print(42)       // T inferred as int
Print("hello")  // T inferred as string
\`\`\`

### Constraints

Constraints restrict which types a type parameter can accept. The \`any\` constraint allows everything. The \`comparable\` constraint allows types that support \`==\` and \`!=\`:

\`\`\`go
func Contains[T comparable](slice []T, target T) bool {
    for _, v := range slice {
        if v == target {
            return true
        }
    }
    return false
}
\`\`\`

You can also define your own constraint interfaces:

\`\`\`go
type Number interface {
    int | float64
}

func Sum[T Number](nums []T) T {
    var total T
    for _, n := range nums {
        total += n
    }
    return total
}
\`\`\`

### Multiple Type Parameters

A function can have more than one type parameter:

\`\`\`go
func Map[T any, U any](slice []T, f func(T) U) []U {
    result := make([]U, len(slice))
    for i, v := range slice {
        result[i] = f(v)
    }
    return result
}
\`\`\`

### Your Task

Write a generic function \`Filter[T any]\` that takes a \`[]T\` and a \`func(T) bool\` predicate. It should return a new \`[]T\` containing only the elements for which the predicate returns \`true\`.`,

  starterCode: `package main

import "fmt"

func Filter[T any](slice []T, predicate func(T) bool) []T {
\t// Your code here
\treturn nil
}

func main() {
\tnums := []int{1, 2, 3, 4, 5, 6}
\tevens := Filter(nums, func(n int) bool {
\t\treturn n%2 == 0
\t})
\tfmt.Println(evens)

\twords := []string{"hi", "hello", "hey", "greetings"}
\tshort := Filter(words, func(s string) bool {
\t\treturn len(s) <= 3
\t})
\tfmt.Println(short)
}
`,

  solution: `package main

import "fmt"

func Filter[T any](slice []T, predicate func(T) bool) []T {
\tvar result []T
\tfor _, v := range slice {
\t\tif predicate(v) {
\t\t\tresult = append(result, v)
\t\t}
\t}
\treturn result
}

func main() {
\tnums := []int{1, 2, 3, 4, 5, 6}
\tevens := Filter(nums, func(n int) bool {
\t\treturn n%2 == 0
\t})
\tfmt.Println(evens)

\twords := []string{"hi", "hello", "hey", "greetings"}
\tshort := Filter(words, func(s string) bool {
\t\treturn len(s) <= 3
\t})
\tfmt.Println(short)
}
`,

  tests: [
    {
      name: "Filter ints (keep evens)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tnums := []int{1, 2, 3, 4, 5, 6}
\tevens := Filter(nums, func(n int) bool {
\t\treturn n%2 == 0
\t})
\tfmt.Println(evens)
}`,
      expected: "[2 4 6]\n",
    },
    {
      name: "Filter strings (keep short ones)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\twords := []string{"hi", "hello", "hey", "greetings"}
\tshort := Filter(words, func(s string) bool {
\t\treturn len(s) <= 3
\t})
\tfmt.Println(short)
}`,
      expected: "[hi hey]\n",
    },
    {
      name: "Empty result when nothing matches",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tnums := []int{1, 3, 5, 7}
\tevens := Filter(nums, func(n int) bool {
\t\treturn n%2 == 0
\t})
\tfmt.Println(evens)
}`,
      expected: "[]\n",
    },
  ],
};
