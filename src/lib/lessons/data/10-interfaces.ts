import type { Lesson } from "../types";

export const interfaces: Lesson = {
  id: "interfaces",
  title: "Interfaces",
  chapterId: "structs-interfaces",
  content: `## Implicit Contracts

Interfaces in Go define behavior. An interface is a set of method signatures. Any type that implements all the methods of an interface automatically satisfies it. No \`implements\` keyword needed.

\`\`\`go
type Shape interface {
    Area() float64
}
\`\`\`

Any type with an \`Area() float64\` method satisfies \`Shape\`. The type does not even need to know the interface exists.

### Why This Matters

This design means you can define interfaces *after* the concrete types are written. You can define an interface in your package that is satisfied by types from a third-party library, without modifying that library.

This is fundamentally different from Java or C# where implementing an interface is an explicit declaration. Go's approach is called *structural typing*.

### Using Interfaces

Interfaces let you write functions that accept any type with the right behavior:

\`\`\`go
func printArea(s Shape) {
    fmt.Printf("Area: %.2f\\n", s.Area())
}
\`\`\`

This function works with circles, rectangles, triangles, or anything else that has an \`Area()\` method.

### The Stringer Interface

The \`fmt\` package defines a commonly used interface:

\`\`\`go
type Stringer interface {
    String() string
}
\`\`\`

If your type implements \`String()\`, the \`fmt\` functions will use it automatically:

\`\`\`go
type Point struct { X, Y int }

func (p Point) String() string {
    return fmt.Sprintf("(%d, %d)", p.X, p.Y)
}

fmt.Println(Point{1, 2}) // prints "(1, 2)"
\`\`\`

### The Empty Interface

The type \`interface{}\` (or its alias \`any\` since Go 1.18) has no methods, so every type satisfies it. It is Go's version of "accept anything":

\`\`\`go
func describe(i any) {
    fmt.Printf("(%v, %T)\\n", i, i)
}
\`\`\`

Use it sparingly. Overusing \`any\` throws away the type safety that makes Go reliable.

### Your Task

Define a \`Shape\` interface with a single method: \`Area() float64\`.

Define two types:
- \`Circle\` with a \`Radius float64\` field
- \`Square\` with a \`Side float64\` field

Implement \`Area()\` on both types. The area of a circle is \`math.Pi * r * r\`.

Write a function \`totalArea\` that takes a \`[]Shape\` and returns the sum of all areas.`,

  starterCode: `package main

import (
\t"fmt"
\t"math"
)

// Define Shape interface

// Define Circle struct and Area method

// Define Square struct and Area method

// Write totalArea function

func main() {
\tshapes := []Shape{
\t\tCircle{Radius: 5},
\t\tSquare{Side: 3},
\t\tCircle{Radius: 2},
\t}
\tfmt.Printf("%.2f\\n", totalArea(shapes))
}
`,

  solution: `package main

import (
\t"fmt"
\t"math"
)

type Shape interface {
\tArea() float64
}

type Circle struct {
\tRadius float64
}

func (c Circle) Area() float64 {
\treturn math.Pi * c.Radius * c.Radius
}

type Square struct {
\tSide float64
}

func (s Square) Area() float64 {
\treturn s.Side * s.Side
}

func totalArea(shapes []Shape) float64 {
\ttotal := 0.0
\tfor _, s := range shapes {
\t\ttotal += s.Area()
\t}
\treturn total
}

func main() {
\tshapes := []Shape{
\t\tCircle{Radius: 5},
\t\tSquare{Side: 3},
\t\tCircle{Radius: 2},
\t}
\tfmt.Printf("%.2f\\n", totalArea(shapes))
}
`,

  tests: [
    {
      name: "Circle area (r=5)",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tc := Circle{Radius: 5}
\tfmt.Printf("%.2f\\n", c.Area())
}`,
      expected: "78.54\n",
    },
    {
      name: "Square area (side=3)",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\ts := Square{Side: 3}
\tfmt.Printf("%.2f\\n", s.Area())
}`,
      expected: "9.00\n",
    },
    {
      name: "totalArea with mixed shapes",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tshapes := []Shape{
\t\tCircle{Radius: 5},
\t\tSquare{Side: 3},
\t\tCircle{Radius: 2},
\t}
\tfmt.Printf("%.2f\\n", totalArea(shapes))
}`,
      expected: "100.11\n",
    },
  ],
};
