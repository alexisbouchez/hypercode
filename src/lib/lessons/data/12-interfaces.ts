import type { Lesson } from "../types";

export const interfaces: Lesson = {
  id: "interfaces",
  title: "Interfaces",
  chapterId: "custom-types",
  content: `## Implicit Contracts

Interfaces in Go define behavior. An interface is a set of method signatures. Any type that implements all the methods of an interface automatically satisfies it. No \`implements\` keyword needed.

\`\`\`go
type Shape interface {
    Area() float64
}
\`\`\`

Any type with an \`Area() float64\` method satisfies \`Shape\`. The type does not even need to know the interface exists.

> "All stations, report!" When the captain calls for status, every department responds the same way --- even though Engineering and Medical do very different things. That is what an interface does: one contract, many implementations.

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
func printAny(i any) {
    fmt.Printf("(%v, %T)\\n", i, i)
}
\`\`\`

Use it sparingly. Overusing \`any\` throws away the type safety that makes Go reliable.

### Type Assertions

When you have a value of type \`any\` (or any interface), you can extract the underlying concrete value using a **type assertion**:

\`\`\`go
var i any = "hello"

s, ok := i.(string)  // s = "hello", ok = true
n, ok := i.(int)     // n = 0, ok = false
\`\`\`

Always use the comma-ok form. Without it, a failed type assertion panics:

\`\`\`go
s := i.(string) // works
n := i.(int)    // panic: interface conversion
\`\`\`

### Type Switches

A **type switch** lets you branch based on the concrete type of an interface value:

\`\`\`go
func classify(i any) string {
    switch v := i.(type) {
    case string:
        return "string: " + v
    case int:
        return fmt.Sprintf("int: %d", v)
    case bool:
        return fmt.Sprintf("bool: %v", v)
    default:
        return "unknown"
    }
}
\`\`\`

Inside each case, \`v\` is already the correct type — no further assertion needed. Type switches are cleaner than chains of type assertions when you need to handle multiple types.

### Embedded Interfaces

Go interfaces can be composed by embedding one interface inside another. This is how you build larger contracts from smaller ones:

\`\`\`go
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

type ReadWriter interface {
    Reader
    Writer
}
\`\`\`

\`ReadWriter\` requires both \`Read\` and \`Write\`. Any type satisfying \`ReadWriter\` also satisfies \`Reader\` and \`Writer\` individually. This is exactly how the standard library's \`io.ReadWriter\` is defined.

You can mix embedded interfaces with additional methods:

\`\`\`go
type ReadCloser interface {
    Reader
    Close() error
}
\`\`\`

This keeps interfaces small and composable. The Go proverb is: *"The bigger the interface, the weaker the abstraction."* Start with one-method interfaces and compose them.

### Your Task

Define a \`Shape\` interface with a single method: \`Area() float64\`.

Define a \`Describer\` interface with a single method: \`Describe() string\`.

Define a \`DetailedShape\` interface that embeds both \`Shape\` and \`Describer\`.

Define two types:
- \`Circle\` with a \`Radius float64\` field
- \`Square\` with a \`Side float64\` field

Implement \`Area()\` and \`Describe()\` on both types. The area of a circle is \`math.Pi * r * r\`.
- \`Circle.Describe()\` returns \`"circle with radius X.XX"\`
- \`Square.Describe()\` returns \`"square with side X.XX"\`

Write a function \`totalArea\` that takes a \`[]Shape\` and returns the sum of all areas.

Write a function \`describeShape\` that takes a \`Shape\` and returns a string using a type switch:
- For a \`Circle\`, return \`"circle with radius X.XX"\`
- For a \`Square\`, return \`"square with side X.XX"\`
- For anything else, return \`"unknown shape"\`

Write a function \`printDetailed\` that takes a \`DetailedShape\` and returns a string in the format \`"DESCRIBE = AREA"\`, for example \`"circle with radius 5.00 = 78.54"\`.

Use \`fmt.Sprintf("circle with radius %.2f", ...)\` for formatting.`,

  starterCode: `package main

import (
\t"fmt"
\t"math"
)

// Define Shape interface

// Define Describer interface

// Define DetailedShape interface (embed Shape and Describer)

// Define Circle struct with Area and Describe methods

// Define Square struct with Area and Describe methods

// Write totalArea function

// Write describeShape function using a type switch

// Write printDetailed function

func main() {
\tshapes := []Shape{
\t\tCircle{Radius: 5},
\t\tSquare{Side: 3},
\t}
\tfmt.Printf("%.2f\\n", totalArea(shapes))
\tfor _, s := range shapes {
\t\tfmt.Println(describeShape(s))
\t}
\tvar ds DetailedShape = Circle{Radius: 5}
\tfmt.Println(printDetailed(ds))
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

type Describer interface {
\tDescribe() string
}

type DetailedShape interface {
\tShape
\tDescriber
}

type Circle struct {
\tRadius float64
}

func (c Circle) Area() float64 {
\treturn math.Pi * c.Radius * c.Radius
}

func (c Circle) Describe() string {
\treturn fmt.Sprintf("circle with radius %.2f", c.Radius)
}

type Square struct {
\tSide float64
}

func (s Square) Area() float64 {
\treturn s.Side * s.Side
}

func (s Square) Describe() string {
\treturn fmt.Sprintf("square with side %.2f", s.Side)
}

func totalArea(shapes []Shape) float64 {
\ttotal := 0.0
\tfor _, s := range shapes {
\t\ttotal += s.Area()
\t}
\treturn total
}

func describeShape(s Shape) string {
\tswitch v := s.(type) {
\tcase Circle:
\t\treturn fmt.Sprintf("circle with radius %.2f", v.Radius)
\tcase Square:
\t\treturn fmt.Sprintf("square with side %.2f", v.Side)
\tdefault:
\t\treturn "unknown shape"
\t}
}

func printDetailed(ds DetailedShape) string {
\treturn fmt.Sprintf("%s = %.2f", ds.Describe(), ds.Area())
}

func main() {
\tshapes := []Shape{
\t\tCircle{Radius: 5},
\t\tSquare{Side: 3},
\t}
\tfmt.Printf("%.2f\\n", totalArea(shapes))
\tfor _, s := range shapes {
\t\tfmt.Println(describeShape(s))
\t}
\tvar ds DetailedShape = Circle{Radius: 5}
\tfmt.Println(printDetailed(ds))
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
    {
      name: "describeShape Circle",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tfmt.Println(describeShape(Circle{Radius: 5}))
}`,
      expected: "circle with radius 5.00\n",
    },
    {
      name: "describeShape Square",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tfmt.Println(describeShape(Square{Side: 3}))
}`,
      expected: "square with side 3.00\n",
    },
    {
      name: "printDetailed with Circle",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tvar ds DetailedShape = Circle{Radius: 5}
\tfmt.Println(printDetailed(ds))
}`,
      expected: "circle with radius 5.00 = 78.54\n",
    },
    {
      name: "printDetailed with Square",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tvar ds DetailedShape = Square{Side: 4}
\tfmt.Println(printDetailed(ds))
}`,
      expected: "square with side 4.00 = 16.00\n",
    },
    {
      name: "Circle satisfies DetailedShape via embedding",
      code: `package main

import (
\t"fmt"
\t"math"
)

{{FUNC}}

func main() {
\tvar ds DetailedShape = Circle{Radius: 3}
\t// Can use as Shape
\tvar s Shape = ds
\tfmt.Printf("%.2f\\n", s.Area())
\t// Can use as Describer
\tvar d Describer = ds
\tfmt.Println(d.Describe())
}`,
      expected: "28.27\ncircle with radius 3.00\n",
    },
  ],
};
