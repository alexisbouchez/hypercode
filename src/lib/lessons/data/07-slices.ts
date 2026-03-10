import type { Lesson } from "../types";

export const slices: Lesson = {
  id: "slices",
  title: "Slices",
  chapterId: "data-structures",
  content: `## Dynamic Arrays, Done Right

Go has arrays, but you will rarely use them directly. Arrays have a fixed size baked into their type: \`[5]int\` and \`[10]int\` are different types entirely. Instead, Go gives you **slices**: a flexible, dynamic view over an underlying array.

### Creating Slices

\`\`\`go
// Slice literal
numbers := []int{1, 2, 3, 4, 5}

// Make a slice with initial length and capacity
data := make([]int, 5)      // len=5, cap=5, filled with zeros
buffer := make([]int, 0, 10) // len=0, cap=10
\`\`\`

The difference between \`[5]int\` (array) and \`[]int\` (slice) is that single missing number. Slices are what you want almost every time.

### Length and Capacity

Every slice has two properties: **length** (how many elements it contains) and **capacity** (how many elements the underlying array can hold before reallocation).

\`\`\`go
s := make([]int, 3, 10)
fmt.Println(len(s)) // 3
fmt.Println(cap(s)) // 10
\`\`\`

> "Tea, Earl Grey, hot." The replicator does not give you the whole cargo bay --- just the slice you asked for. Go slices work the same way: a precise view into a larger array.

### Append

\`append\` adds elements to a slice and returns a new slice. If the underlying array is full, Go allocates a bigger one and copies the data:

\`\`\`go
s := []int{1, 2, 3}
s = append(s, 4, 5)
// s is now [1, 2, 3, 4, 5]
\`\`\`

Always reassign the result of \`append\` back to the slice variable. This is not optional. \`append\` may return a slice pointing to a completely new array.

### Slicing

You can create a new slice from an existing one using the slice operator:

\`\`\`go
s := []int{0, 1, 2, 3, 4}
a := s[1:3]  // [1, 2]     (from index 1, up to but not including 3)
b := s[:3]   // [0, 1, 2]  (from the start)
c := s[2:]   // [2, 3, 4]  (to the end)
\`\`\`

A sub-slice shares the same underlying array. Modifying one affects the other. If you need an independent copy, use \`copy\` or \`append\` to a new slice.

### The copy Builtin

\`copy\` copies elements from a source slice into a destination slice and returns the number of elements copied. The destination must already have enough length --- \`copy\` does not grow the slice:

\`\`\`go
src := []int{1, 2, 3, 4, 5}
dst := make([]int, 3)
n := copy(dst, src)
fmt.Println(dst) // [1 2 3]
fmt.Println(n)   // 3 (copied min(len(dst), len(src)) elements)
\`\`\`

To make a full independent copy of a slice:

\`\`\`go
original := []int{10, 20, 30}
clone := make([]int, len(original))
copy(clone, original)
// Modifying clone does not affect original
\`\`\`

### Nil Slices vs Empty Slices

This is a subtle but important distinction:

\`\`\`go
var nilSlice []int          // nil slice: no underlying array
emptySlice := []int{}       // empty slice: has an underlying array, but length 0
madeSlice := make([]int, 0) // also empty, not nil

fmt.Println(nilSlice == nil)    // true
fmt.Println(emptySlice == nil)  // false
fmt.Println(len(nilSlice))      // 0
fmt.Println(len(emptySlice))    // 0
\`\`\`

Both have length 0 and both work fine with \`append\`, \`range\`, and \`len\`. The difference matters when you serialize to JSON (\`nil\` becomes \`null\`, empty becomes \`[]\`) or check for "no data provided" vs "explicitly empty."

In practice: use \`var s []int\` (nil) when you might not need a slice at all, and \`s := []int{}\` when you need an explicitly empty collection.

### Your Task

Write a function \`sum\` that takes a \`[]int\` and returns the sum of all elements.

Write a function \`filterEvens\` that takes a \`[]int\` and returns a new \`[]int\` containing only the even numbers, in order.

Write a function \`cloneSlice\` that takes a \`[]int\` and returns an independent copy using the \`copy\` builtin. Modifying the returned slice must not affect the original.`,

  starterCode: `package main

import "fmt"

func sum(numbers []int) int {
\t// Your code here
\treturn 0
}

func filterEvens(numbers []int) []int {
\t// Return only even numbers
\treturn nil
}

func cloneSlice(s []int) []int {
\t// Return an independent copy of s using the copy builtin
\treturn nil
}

func main() {
\tnums := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
\tfmt.Println(sum(nums))
\tfmt.Println(filterEvens(nums))

\toriginal := []int{10, 20, 30}
\tcloned := cloneSlice(original)
\tfmt.Println(cloned)
}
`,

  solution: `package main

import "fmt"

func sum(numbers []int) int {
\ttotal := 0
\tfor _, n := range numbers {
\t\ttotal += n
\t}
\treturn total
}

func filterEvens(numbers []int) []int {
\tvar result []int
\tfor _, n := range numbers {
\t\tif n%2 == 0 {
\t\t\tresult = append(result, n)
\t\t}
\t}
\treturn result
}

func cloneSlice(s []int) []int {
\tclone := make([]int, len(s))
\tcopy(clone, s)
\treturn clone
}

func main() {
\tnums := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
\tfmt.Println(sum(nums))
\tfmt.Println(filterEvens(nums))

\toriginal := []int{10, 20, 30}
\tcloned := cloneSlice(original)
\tfmt.Println(cloned)
}
`,

  tests: [
    {
      name: "sum of 1..10",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(sum([]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))
}`,
      expected: "55\n",
    },
    {
      name: "sum of empty slice",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(sum([]int{}))
}`,
      expected: "0\n",
    },
    {
      name: "filterEvens from 1..10",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(filterEvens([]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))
}`,
      expected: "[2 4 6 8 10]\n",
    },
    {
      name: "filterEvens with no evens",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(filterEvens([]int{1, 3, 5}))
}`,
      expected: "[]\n",
    },
    {
      name: "cloneSlice returns correct values",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(cloneSlice([]int{10, 20, 30}))
}`,
      expected: "[10 20 30]\n",
    },
    {
      name: "cloneSlice is independent from original",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\toriginal := []int{1, 2, 3}
\tcloned := cloneSlice(original)
\tcloned[0] = 999
\tfmt.Println(original)
\tfmt.Println(cloned)
}`,
      expected: "[1 2 3]\n[999 2 3]\n",
    },
  ],
};
