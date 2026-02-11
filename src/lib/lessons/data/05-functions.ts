import type { Lesson } from "../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions in Go

Functions are declared with \`func\`, followed by the name, parameters, and return type:

\`\`\`go
func greet(name string) string {
    return "Hello, " + name
}
\`\`\`

Parameter types come *after* the name, not before. This was a deliberate choice. The Go team believes it reads more naturally, especially as declarations get complex.

When consecutive parameters share a type, you can group them:

\`\`\`go
func add(a, b int) int {
    return a + b
}
\`\`\`

### Multiple Return Values

This is one of Go's most distinctive features. A function can return more than one value:

\`\`\`go
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, fmt.Errorf("division by zero")
    }
    return a / b, nil
}
\`\`\`

The caller unpacks the results:

\`\`\`go
result, err := divide(10, 3)
\`\`\`

This pattern is the foundation of Go's error handling. Instead of exceptions, functions return errors as values.

### Named Return Values

You can name your return values, which documents what the function returns and allows "naked" returns:

\`\`\`go
func swap(a, b string) (first, second string) {
    first = b
    second = a
    return // returns first and second
}
\`\`\`

Use named returns sparingly. They are most useful for documenting return values in short functions. In longer functions, explicit returns are clearer.

### Functions as Values

Functions are first-class values. You can assign them to variables, pass them as arguments, and return them from other functions:

\`\`\`go
double := func(x int) int {
    return x * 2
}
fmt.Println(double(5)) // 10
\`\`\`

### Your Task

Write a function \`minMax\` that takes a \`[]int\` and returns two \`int\` values: the minimum and the maximum values from the slice.`,

  starterCode: `package main

import "fmt"

func minMax(numbers []int) (int, int) {
\t// Your code here
\treturn 0, 0
}

func main() {
\tmin, max := minMax([]int{3, 1, 4, 1, 5, 9, 2, 6})
\tfmt.Printf("min: %d, max: %d\\n", min, max)
}
`,

  solution: `package main

import "fmt"

func minMax(numbers []int) (int, int) {
\tmin, max := numbers[0], numbers[0]
\tfor _, n := range numbers {
\t\tif n < min {
\t\t\tmin = n
\t\t}
\t\tif n > max {
\t\t\tmax = n
\t\t}
\t}
\treturn min, max
}

func main() {
\tmin, max := minMax([]int{3, 1, 4, 1, 5, 9, 2, 6})
\tfmt.Printf("min: %d, max: %d\\n", min, max)
}
`,

  tests: [
    {
      name: "minMax with mixed values",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tmin, max := minMax([]int{3, 1, 4, 1, 5, 9, 2, 6})
\tfmt.Printf("min: %d, max: %d\\n", min, max)
}`,
      expected: "min: 1, max: 9\n",
    },
    {
      name: "minMax with single element",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tmin, max := minMax([]int{42})
\tfmt.Printf("min: %d, max: %d\\n", min, max)
}`,
      expected: "min: 42, max: 42\n",
    },
    {
      name: "minMax with negative values",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tmin, max := minMax([]int{-3, -1, -4})
\tfmt.Printf("min: %d, max: %d\\n", min, max)
}`,
      expected: "min: -4, max: -1\n",
    },
    {
      name: "minMax with identical values",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tmin, max := minMax([]int{5, 5, 5})
\tfmt.Printf("min: %d, max: %d\\n", min, max)
}`,
      expected: "min: 5, max: 5\n",
    },
  ],
};
